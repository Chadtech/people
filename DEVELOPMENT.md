# Development guide

See the [README](README.md) for a project overview and first-run setup. This
guide covers the build workflow, tests, and implementation details.

## Build and watch


| Command | Purpose |
| --- | --- |
| `make dev` | Watch Elm and Acadia sources and serve the app |
| `make build` | Generate Elm/Haskell bindings and compile `index.html` |
| `make serve` | Build once and start Acadia |
| `make worker` | Build the app, bindings, and Haskell worker |
| `make dev-data` | Create the private fixture file if missing |

Saving an Elm file recompiles `index.html` without restarting Acadia. Refresh
the browser to load the result. Saving a `.db` file regenerates both sets of
bindings, recompiles Elm, and restarts Acadia. A failed rebuild reports the
compiler error and waits for the next save; the server returns after a successful
rebuild. Press Ctrl-C to stop the watchers and Acadia.

The watcher manages Acadia and Elm. After schema changes, also rebuild and
restart the worker with `make worker` and the run command in the
[README](README.md#enable-ai-replies). Existing in-memory state is not migrated.

Person routes are `/person/all`, `/person/new`, and `/person/<id>`;
conversations are at `/conversation`. Database operations use `/_endpoints`.

### Checks

Run offline unit tests after generating the bindings with `make worker`:

```sh
cabal test people-unit
```

The lifecycle integration executable creates test records. Use an isolated
Acadia server, with no AI worker attached:

```sh
acadia serve --socket=localhost:9011 --html=index.html
```

In another terminal, run:

```sh
cabal run people-integration -- http://localhost:9011
```

Add `--tools-only` after the URL to run only the tool-loop checks. The suites
cover protocol validation, context selection, stale edits, action ownership,
cancellation, duplicate calls, competing workers, scheduling, and expired-turn
recovery using deterministic model responses. Live OpenAI behavior and the
quality of appraisal predictions require separate evaluation.

## AI turn behavior

### Third-person appraisal and narration

Each turn first asks an outside observer to appraise the selected person's
identity, goals, memories, conversation, and private drive state. It returns
structured competing predictions with evidence and uncertainty and has no tools.
A deterministic selector scores proposals by likelihood plus weighted support
from affiliation, curiosity, and competence; ties use proposal order. This is an
experimental behavioral model.

Narration starts a separate Responses history with ordinary context plus the
selected action category and coarse feeling. Private motives, evidence,
uncertainty, drive scores, and observer reasoning stay outside that history.
The inclination guides the reply while facts and commitments still apply.
Self-model revision is deferred. Appraisal adds one paid call per turn; invalid
appraisals fail the turn.

Dispositions start at 50, range from 0 to 100, and change by at most one point
per completed turn. Their deltas and the public reply commit in one transaction.
Failed or cancelled turns do not update dispositions; concurrent completions
apply deltas to the current stored values. Dispositions share the session's
in-memory lifetime.

### Context and actions

The context budget is 24,000 UTF-8 bytes, with 512 bytes reserved for cognitive
inputs. Identity, active goals, and the latest message must fit or the turn fails.
Relevant memories receive up to one third of the remaining budget, capped at
6,000 bytes. Older history uses the rest, including unused memory allowance.
Snapshots record the exact stage requests, selection reasons, and omissions;
later human messages do not change a response already in flight.

The narrator can call `create_goal(description)`, `complete_goal(goal_id)`, and
`save_memory(content)` for its own records. Each action commits separately and
returns its saved ID or a validation error before generation continues. Goal IDs
are decimal strings. Only the final response without tool calls becomes a public
message. The worker replays Responses output items, including reasoning, with
`function_call_output` results and `store: false`.

Turns allow at most eight tool calls and 175 seconds under a three-minute claim.
Actions require an active, unexpired generation. Identical repeated call IDs
reuse the saved result; ambiguous database or transport failures stop the turn
without retrying a write. OpenAI failures stop the remaining run budget.
Stop rejects subsequent actions and late replies, while committed actions stay
saved. Inspect the transcript before starting a fresh turn after a failure.
Prompt snapshots exclude the API key.

A later worker poll marks expired turns failed and pauses autonomy instead of
repeating an uncertain paid call. Start a fresh turn or re-enable autonomy to
continue.

## Implementation notes

`Person.db`, `Goal.db`, and `Memory.db` own their domains; `Origin.db` shares
curated/AI attribution. `Conversation.db` owns participants and run settings;
`Generation.db` owns generation records and claims. `Chat.db` coordinates
transactional message and turn operations. `Disposition.db` and `CognitiveTrace.db`
represent private drive state and appraisal traces. Human and AI profile pages
have separate Elm models; `View.PersonProfile` shares common presentation.
Distinct domain types survive generated Elm/Haskell bindings, with Haskell
instances in `People.DomainInstances`. Generated files stay generator-owned.

Acadia 0.3.0 has several runtime constraints to preserve when changing queries:

- Responses require top-level `Rows`. Combined page loads use tagged,
  left-associated `union` queries with disjoint keys and conversation filtering
  after the union. Keep `union`: `xunion` dropped person-only results. Nested
  `Rows` records and other join shapes failed in runtime checks.
- `Chat.getConversationPageFlags` supplies the conversation and its four
  collections before Elm page initialization. Polling reuses the same endpoint;
  prompt snapshots remain on demand.
  `Conversation.getAllConversationsPageFlags` similarly combines conversation
  and person rows. Avoid unused combined constructors that deepen Elm decoders.
- Database helpers check update preconditions explicitly. `Store.require` fails
  for a missing row, `ensure` checks existence, and `update` checks before writing.
  Handle optional empty strings before SQL row expressions to avoid NULL binding.
- `server/src/Acadia/Bytes/Encode.hs` overrides the generated runtime to encode
  endpoint string lengths in UTF-8 bytes. Keep this compatibility copy aligned
  with future Acadia upgrades. Integration checks cover Unicode requests and
  context selection under history pressure.

## Local configuration and sample data

The worker reads exported environment variables first, then `.env.local`, then
`.env`, from the project root. Both files are Git-ignored. They accept
`KEY=value`, optional `export`, single or double quotes, and comments. Values
are literal; shell commands, variable interpolation, escape sequences, and
multiline values are not evaluated. Restart the worker after changing them.
The connection `check` command needs no API credentials.

`Fixtures.fillDevelopmentData`, called through `src/DevelopmentData.elm`, seeds
the original six sample people, Ada and Sam with their goals and memories, and
their shared conversation. One transaction commits the data and fixture marker.
Repeated or concurrent calls preserve edits and accumulated state without
creating duplicates. The marker resets with the database; the next app load
recreates the starting data. Conversations refreshes after setup; All persons
may need revisiting after the first fill.

Edit private starting conditions in `src/Fixtures.db`. The Make targets create
this Git-ignored file from `src/Fixtures.db.example` only when missing. Keep the
tracked example free of private data. Run `make dev-data` before invoking Acadia
directly. Saving the local fixture file during `make dev` restarts Acadia and
resets the session.

All local browser sessions share one server-owned human account. Sending a
message joins that human to the conversation if needed; the client cannot
choose a different human author. AI requests check participant membership and
exclude humans from scheduling. Acadia endpoints use unrestricted local access;
these checks do not provide multi-user authentication or permissions.
