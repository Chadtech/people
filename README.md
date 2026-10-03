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
cabal run exe:people-worker -- check http://localhost:9000
```

Run the worker from the project root. It automatically reads `.env.local` and
`.env` there. Exported shell variables take priority, followed by `.env.local`,
then `.env`. Missing files are fine; both local files are Git-ignored.

If you already have `.env.local`, fill in `OPENAI_API_KEY` and `OPENAI_MODEL`
there. Otherwise, copy `.env.example` to `.env` and fill in those two values.
The model must support the OpenAI Responses API and function calling.
Files support `KEY=value`, optional `export`, single or double quotes, and
comments. Values are literal: shell commands, variable interpolation, escape
sequences, and multiline values are not evaluated. Restart the worker after
editing a file. The connection `check` command needs no API configuration.

Start the worker (in a separate terminal from `make dev`) with:

```sh
cabal run exe:people-worker -- run http://localhost:9000
```

Search for people in the creation form and select a result to add them to the
list of people joining. Remove anyone from that list before creating the conversation;
including the local human is optional. Creation adds the selected people before opening the
conversation. If adding a participant fails, the form offers a retry that resumes
adding the remaining people to the same conversation.

On a conversation page, the roster stays visible above the view tabs. The Person
menu contains eligible participants and defaults to the local human when included,
otherwise to an AI participant, using saved profile names. Adding someone uses a
separate Participant to add menu and preserves the selected speaker. Edit human profile opens the name editor; new
profiles are seeded as Chadtech by the development fixtures. Missing local accounts
or unresolved message authors fail instead of receiving placeholder names. Type a message and choose Send to post under
that name, without queuing a model call. Select an AI person
and choose Let selected person reply to request a response. Switching the selection
preserves an unsent human draft. You can post while an AI is generating; an
already-running reply finishes using its saved context. Autonomy, if enabled,
continues on its existing schedule.

`Person.kind` is `HumanPerson | AiPerson AiPersonProfile`: only the AI person
variant contains identity and aspirations. Humans have no empty or optional AI profile fields. Human
profile pages show common details; AI pages keep their profile, goals, and
memory editors. The backend rejects AI-profile updates, goals, and memories
for human people. Every message has an explicit
`PersonId` author. `LocalAccount` resolves the server-owned local human account;
`Chat.sendMessage` never accepts a client-supplied author. Conversations include
the local human on creation, and sending joins the human to an older conversation.
`Chat.requestTurn` only queues AI work, checks membership and AI eligibility, and
never posts a human message. The worker also excludes humans from its scheduler.
Prompt snapshots retain the included message IDs and exact context: later human
messages do not silently change a response already in flight.

This is still the local, single-account MVP. The account has a stable identity
for the lifetime of the current Acadia database, including browser reloads and
worker restarts. All local browser sessions currently act as that same account.
Multi-user authentication, invitations, membership permissions, and durable
storage remain future work; these unrestricted local endpoints are not a
multi-user authorization system. Schema changes require rebuilding both clients
and restarting Acadia; its current in-memory state is not migrated.

Give a person an identity on their page, then create a conversation and select
its participants. Individual replies and bounded runs are queued in Acadia.
Enable autonomy for one turn every five minutes; Stop disables it. The interval,
next due time, run budget, and last speaker live in Acadia tables. Missed
intervals produce one turn when the worker returns, not a catch-up burst.
The worker saves the assembled request and context selection in the generation
record, then calls OpenAI. Public replies are ordinary text. The model can call
`create_goal(description)`, `complete_goal(goal_id)`, and `save_memory(content)`;
each action commits separately and returns a success result (including its saved
record ID) or a validation error before the model continues. Goal IDs are decimal
strings. Tools can only affect the selected AI person's records and require an
active, unexpired generation. Only the final response without tool calls becomes
a conversation message.

The worker follows the [Responses function-calling protocol](https://developers.openai.com/api/docs/guides/function-calling),
replaying output items, including reasoning, with `function_call_output` results
and `store: false`. The prompt inspector's raw snapshot includes all requests and
the tool transcript. The loop allows at most eight tool calls and 175 seconds per
turn. Repeated call IDs with identical arguments reuse their result. Ambiguous
database/transport failures stop the turn without retrying a write.

Stop rejects subsequent actions and late replies. Actions already committed stay
saved if the reply fails, times out, or is cancelled. Inspect the saved goals,
memories, and turn transcript before starting a fresh turn.
OpenAI failures stop the remaining run budget rather than silently retrying paid
requests. Prompt snapshots exclude the API key.

Person pages let you add goals and memories, change goal status, and retire
memories. AI reflections retain their originating turn and are treated as
fallible context. Use Refresh to see changes made by ongoing AI turns.

The current context budget is measured in UTF-8 bytes, not tokenizer counts.
Identity, active goals, and the latest message must fit;
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

It checks stale identity edits, duplicate turn claims, tool-result continuation,
action failure recovery, duplicate call IDs, goal ownership, cancellation,
bounded-run failure behavior, competing workers, round-robin autonomy, and
expired-worker recovery without live AI calls. Add `--tools-only` after the URL
to run just the tool-loop checks.

Run `cabal test people-unit` for offline OpenAI response validation, including
refusals, incomplete replies, tool argument validation, reasoning replay, output
limits, and goal ID bounds.

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

Edit your private starting conditions in `src/Fixtures.db`. This file is
Git-ignored; `make build`, `make serve`, `make worker`, and `make dev` create it
from the tracked `src/Fixtures.db.example` only when it is missing, preserving
your local edits. Run `make dev-data` first if you invoke `acadia` directly.
Keep the example free of private data. `make dev` watches the ignored local
fixture file too, so saving it rebuilds and restarts the in-memory server.
The original six people are also seeded by that one endpoint. Page reads still run alongside setup;
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
memories. `Conversation.db` owns conversation records, participants, creation,
queries, and run/autonomy settings. `Generation.db` owns generation records,
phase and prompt queries, claims, and lease queries. `Chat.db` owns messages and
coordinates atomic turn requests, completion, failure, and
cancellation across those modules. Their shared backend tables are ordinary
exports, not client endpoints; the coordinating operations remain transactions.
`Fixtures.fillDevelopmentData` is the only seeding endpoint and uses one marker.
Elm human and AI profiles use independent `HumanPersonPage.elm` and `AiPersonPage.elm`
modules, selected by `Main` from the loaded person kind. Each owns its model and
messages; goals and memories belong to `AiPersonPage`. `View.PersonProfile` shares only
the presentation of common person details.

IDs and values such as message content, revisions, and intervals have
distinct custom types. Haskell instances for generated types live in
`People.DomainInstances`; generated files stay generator-owned. ID instances
support equality, ordering, and decimal display, but deliberately omit `Num`.

Conversation refreshes use two endpoints in `Chat`: `getConversationDetailsFlags`
returns people, messages, participants, and generation summaries;
`getConversationPageFlags` adds the conversation itself. Both reuse
`conversationDetailsRows`, the tagged-row union helper, and response selection.
`Main` loads `getConversationPageFlags` before constructing `ConversationPage`,
passing the conversation and all four collections as its required flags. Page
initialization is pure and makes no requests. Polling reuses the same endpoint
and flag parser. Missing conversations and failed initial loads remain route
states in `Main`. Prompt snapshots remain on demand.

Acadia 0.3.0 requires `Rows` at the response root. These queries must keep unions
left-associated with a single table on the right, use disjoint side tags plus
real row keys, and filter conversation scope after combining the tagged rows.
Other shapes tested here either dropped rows, selected the wrong flag kind, or
failed database startup despite compiling. In particular, replacing these unions
with `xunion` fails the person-only integration case: it drops the person flag
when the other collections are empty. Keep `union`; its `Both` case is unreachable
because the left and right keys start with different Boolean tags. Filtering after the union can scan
more rows than independent filtered queries; revisit this when the runtime's
join handling is fixed. The integration suite compares both responses with the
standalone endpoints for missing conversations, empty collections, overlapping
IDs, populated histories, and another conversation's data.

The conversations index uses `Conversation.getAllConversationsPageFlags`, a single
endpoint returning top-level tagged rows. The Elm page splits them into its
conversation and people lists. Keep this response shape: a record containing
two `Rows` fields is unsupported by Acadia 0.3.0. The integration suite compares
both collections with their standalone endpoints when empty, with only people,
and with both people and conversations.

The tagged query uses disjoint `(False, conversationId)` and `(True, personId)`
keys, so a combined conversation/person row is unreachable. Expose only
`ConversationFlag` and `PersonFlag`; an unused combined constructor generates a
particularly deep Elm decoder. Other used decoders still contain nested calls,
so removing that constructor is not by itself proof that IDE analysis is fixed.

`Store.require` returns a matching row or fails the transaction; `ensure` checks
existence without returning it; `update` checks before changing it. AI profiles
are serialized inside the AI variant. The integration suite verifies empty
profile fields and stale revision rejection against the real runtime.
