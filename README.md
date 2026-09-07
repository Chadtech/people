# people

A small Elm UI backed by the Acadia endpoints in `src/Person.db`.

The [architecture and MVP plan](ARCHITECTURE.md) records the SillyTavern reference,
the recovered earlier discussion, and the next steps toward a virtual space where
AI people can talk and perform in-app activities.

The [code style guide](CODE_STYLE.md) defines Elm import aliases and qualification
rules.

The [design system](DESIGN_SYSTEM.md) records our visual conventions, component
guidelines, and evolving design decisions. Update it as we refine the frontend.

For development, run:

```sh
make dev
```

This requires `watchexec`. Saving an Elm file recompiles `index.html` without
restarting Acadia. Saving an Acadia `.db` file recompiles the database modules,
regenerates the Elm and Haskell bindings, recompiles `index.html`, and restarts the server.
If a rebuild fails, the watcher reports the compiler error and waits for the
next save. The server starts again after the next successful rebuild.
After changing a `.db` file, rebuild and restart the Haskell worker as well so
its endpoint bindings match the served schema. The development watcher manages
Acadia and Elm only.
Refresh the browser to load the newly compiled page. Press Ctrl-C to stop both
watchers and the server.

To build or serve once instead:

```sh
make build
make serve
```

Then open <http://localhost:9000>. People are listed at `/person/all`, created at
`/person/new`, and edited at `/person/<id>`. Conversations are at `/conversation`.
The application uses Acadia's `/_endpoints` route for database operations.

## AI worker (in development)

`make worker` generates both Elm and Haskell bindings and builds the Haskell
worker with GHC 9.8.4. Start Acadia first, then check the connection:

```sh
cabal run people-worker -- check http://localhost:9000
```

Set `OPENAI_API_KEY` and `OPENAI_MODEL` in your local worker environment. The model
must support the OpenAI Responses API and structured JSON outputs. Start with:

```sh
cabal run people-worker -- run http://localhost:9000
```

Give a person an identity on their page, then create a conversation and select
its participants. Individual replies and bounded runs are queued in Acadia.
Enable autonomy for one turn every five minutes; Stop disables it. The interval,
next due time, run budget, and last speaker live in Acadia tables. Missed
intervals produce one turn when the worker returns, not a catch-up burst.
The worker saves the assembled request and context selection in the generation
record, then calls OpenAI. Validated replies, goals, reflections, goal completion,
and shared-note changes are committed together. Each changed note also retains an
immutable revision with its author, generation, and creation time. Load note
history in the conversation to inspect earlier versions. Stop rejects late results.
OpenAI failures stop the remaining run budget rather than silently retrying paid
requests. Prompt snapshots exclude the API key.

Person pages let you add goals and memories, change goal status, and retire
memories. AI reflections retain their originating turn and are treated as
fallible context. Use Refresh to see changes made by ongoing AI turns.

The current context budget is measured in UTF-8 bytes, not tokenizer counts.
Identity, active goals, the shared note, and the latest message must fit;
oversized required context fails visibly instead of silently dropping it.
Relevant memories receive up to one third of the remaining budget, capped at
6,000 bytes. Older history uses the rest, including unused memory allowance.
This prevents long conversations from permanently crowding out recollections.
Prompt snapshots record included and omitted inputs and the memory allowance.
Generation status polls transfer summaries only. Expand a turn and choose
“Load / refresh prompt” to retrieve its saved snapshot; subsequent status polls
do not fetch that snapshot again. Worker cancellation reads one generation phase.
The inspector groups included inputs by source and selection reason, with
separate disclosures for exclusions and the exact saved request.

The current development workflow intentionally uses an in-memory Acadia database.
Registration and restart persistence are not prerequisites for this MVP. Keep
Acadia running for as long as you want to experience an ongoing conversation:
identities, goals, memories, messages, and notes accumulate throughout that run.
Browser reloads preserve the state. Stopping autonomy or restarting only the
Haskell worker also preserves it while Acadia stays up. Restarting Acadia resets
it to the development fixtures; saving a `.db` file with `make dev` also restarts
Acadia, so avoid schema edits during an experience run you want to keep.

Each claimed generation has a deadline of at most three minutes. A later worker
poll marks expired turns failed and pauses their autonomy instead of repeating an
uncertain paid call. Start a fresh turn or re-enable autonomy to continue. Live
OpenAI behavior remains to be verified once the worker is configured.

The lifecycle integration executable creates test records, so run it against an
isolated Acadia server (for example on port 9011):

```sh
cabal run people-integration -- http://localhost:9011
```

It checks stale identity edits, duplicate turn claims, atomic action rollback,
goal ownership, cancellation, bounded-run failure behavior, competing workers,
round-robin autonomy, and expired-worker recovery without live AI calls.

Run `cabal test people-unit` for offline OpenAI response validation, including
refusals, incomplete replies, missing fields, output limits, and goal ID bounds.

## Temporary development data

`Main.init` submits `Fixtures.fillDevelopmentData` through
`src/DevelopmentData.elm`. One Acadia transaction fills the original six sample
people and adds two ready-to-use AIs, Ada and Sam, with distinct identities,
aspirations, initial goals, and curated memories. It also creates their shared
**Long-running conversation**. Autonomy starts off, so loading the app does not
queue paid model calls.

The fixture marker, people, goals, memories, and conversation are committed
together. Repeated calls preserve edits and accumulated state instead of resetting
them. Concurrent attempts cannot create duplicate fixtures. The marker resets
with Acadia's in-memory database; the next app load recreates the starting point.

For a longer experience run, open Conversations, choose **Long-running
conversation**, and enable autonomy when the OpenAI worker is ready. Join in or
stop it whenever you like. Keep the Acadia process alive for that session.

Edit the AI starting conditions in `src/Fixtures.db`; the original six people
are also seeded by that one endpoint. Page reads still run alongside setup;
Conversations refreshes automatically, and All persons may need revisiting after
an initial fill.

## Acadia 0.3.0 compatibility

The Haskell library uses `server/src/Acadia/Bytes/Encode.hs` ahead of the generated
runtime. It is a compatibility copy with UTF-8 byte lengths for endpoint strings;
the generated version counts characters and rejects non-ASCII requests. Keep this
copy aligned with future Acadia runtime upgrades. The integration check sends
long Unicode messages through the actual server and checks memory selection
under history pressure.

Database helpers explicitly check update preconditions because this runtime does
not enforce the documented single-row update behavior. Optional empty strings
are handled before SQL row expressions to avoid binding them as NULL. These
workarounds are localized and covered by lifecycle checks.


## Domain organization and refresh limitation

`Person.db`, `Goal.db`, and `Memory.db` own their respective tables and operations.
`Origin.db` shares curated/AI attribution without a dependency between goals and
memories. `Chat.db` coordinates atomic generation results across those modules.
`Fixtures.fillDevelopmentData` is the only seeding endpoint and uses one marker.
The Elm person page is `PersonPage.elm`; goals and memories are part of that page.

IDs and values such as notes, message content, revisions, and intervals have
distinct custom types. Haskell instances for generated types live in
`People.DomainInstances`; generated files stay generator-owned. ID instances
support equality, ordering, and decimal display, but deliberately omit `Num`.

A single conversation refresh endpoint remains desirable. Acadia 0.3.0 rejects
records containing `Rows` as endpoint results and rejects intermediate `Rows`
transaction bindings. A runtime-tested attempt to combine tagged collections
with full joins also returned no rows: the generated SQL moved per-collection
filters into the final WHERE clause and mishandled nested union presence checks.
The app therefore retains the working separate requests. Do not replace them
with that combined query without testing empty and missing collections as well
as populated conversations. Prompt snapshots and note history remain on demand.

`Store.require` returns a matching row or fails the transaction; `ensure` checks
existence without returning it; `update` checks before changing it. Its
`updateString` helper centralizes the empty-parameter SQL NULL workaround used
by identity updates. The integration suite verifies both empty fields and stale
revision rejection against the real runtime.
