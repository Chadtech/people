# Design system

A living guide for designing and building frontends together. Start here before
changing the UI, and update it when we discover a useful rule or change direction.

Established baseline: People, 2026-09-05. This first edition was checked against
the frontend source; it is not a visual or accessibility audit.

## How to use this document

- **Established** describes the current People design direction and implementation.
- **Guideline** is a starting rule for new work, open to revision through use.
- **Open** identifies a decision or implementation gap we have not resolved.

The general principles can travel to other frontends. The People palette,
typography, and retro styling are project-specific; choose those deliberately
when starting another project. This repository does not automatically configure
other repositories.

User feedback takes precedence. When a decision changes, revise the relevant
section and add a short entry to the decision log. Keep the main guidance current
so readers do not need to reconstruct it from the log.

## General principles — guidelines

1. **Make the task clear.** Give each screen an understandable purpose and make
   the next action easy to find. Show the information needed to make that choice.
2. **Let appearance explain behavior.** Links navigate; buttons perform actions;
   fields invite input. Group related information through spacing and hierarchy.
3. **Use space intentionally.** Keep controls and labels concise, with enough
   breathing room to scan and operate them. Let content determine useful width.
4. **Build on shared patterns.** Reuse existing components and tokens. Add a
   shared variant when a recurring need appears; keep isolated exceptions local.
5. **Design the whole interaction.** Account for focus, loading, empty results,
   success, and errors as well as the initial screen.
6. **Learn from the actual interface.** Check real content in the browser and
   record concrete observations. Treat new aesthetic choices as proposals until
   we have used or reviewed them.

## People visual direction — established

A compact desktop-tool feel, inspired by older Windows interfaces: dark wood
backgrounds, warm gray surfaces, monospace text, and inset/outset edges.
Use restrained texture on the outer background and solid surfaces behind content.
Use bevels to communicate surfaces, editable areas, and pressed controls.

### Color

Values live in [src/Style.elm](src/Style.elm). These are the core existing tokens;
the roles below guide their reuse, not a claim that every pairing is accessible.

| Role | Token/helper | Value |
| --- | --- | --- |
| Outer background | `bgNightwood0` | `#030907` |
| Textured outer background | `bgNightwoodGrain` | Layered gradients over nightwood |
| Input background | `bgNightwood1` | `#071D10` |
| Additional green surfaces | `bgNightwood2`, `bgNightwood3` | `#082208`, `#142909` |
| Panels and secondary controls | `bgGray1` | `#2C2826` |
| Bevel dark edge | internal `gray0Str` | `#131610` |
| Bevel light edge | internal `gray2Str` | `#57524F` |
| Header text | `textGray3` | `#807672` |
| Default text | `textGray4` | `#B0A69A` |
| Links | `link` | `#8CC8FF`; hover/focus `#C4E3FF` |
| Brighter text | `textGray5` | `#E0D6CA` |
| Primary control background | `bgYellow1` | `#302507` |
| Primary control text | `textYellow4` | `#B39F4B` |
| Primary hover/pressed text | `textYellow5` | `#E3D34B` |
| Error text | `textRed1` | `#F21D23` |

Guideline: use yellow emphasis sparingly for the main action. Give errors readable
text explaining the problem and a recovery step; color alone is insufficient.
Check contrast on the actual surface, especially for headers and errors.

### Typography

All text uses the same size: 1rem, including headers, body text, controls, and
status messages. Keep the existing font, with normal weight (400) everywhere;
do not use bold or semibold text. Use spacing and darker header text to establish
hierarchy. There is no muted-text role.

### Spacing and layout

All elements default to zero margin and padding through the global reset.
Apply spacing explicitly with the shared helpers where needed.

The spacing unit is 0.25rem: `p1` is 0.25rem, `p2` is 0.5rem, `p3` is 0.75rem,
and `p4` is 1rem. Gap helpers follow the same scale. At a 16px root size these
are 4, 8, 12, and 16px; helper suffixes are not literal pixel values.

The current shell places a sidebar alongside a flexible scrolling main area.
Sidebar widths are 16rem expanded and 4rem collapsed. The main area uses `minW0`
to allow its flex child to shrink. Pages use the available main-area width, with one outer gutter and one panel
inset; do not cap the whole page to a centered form width.
Pages that need a full viewport minimum height apply `S.minHFullViewport`
explicitly. Reuse the helper rather than styling page children from `Main`.
Use Tailwind-inspired names for reusable layout utilities: `S.flexWrap` maps
to `flex-wrap: wrap`. Keep repeated CSS primitives in `Style`. Reuse
`S.wrapAnywhere` and `S.whitespacePreWrap` for text wrapping, `S.listNone`
for unmarked lists, and `S.border0` for a zero-border reset.
Available responsive helpers are `md` at 768px and `lg` at 1156px.

Guidelines:

- Give each content panel one padding inset; headings, forms, and detail rows
  inside it use container gaps instead of adding more padding. Keep the page's
  outer gutter and controls' internal padding separate from content spacing.
- Use small gaps within a group and larger gaps between groups. Labels and fields
  use consistent gaps. Put fieldset contents in a gap-controlled inner container
  when a heading needs normal layout; native legends sit outside that flow.
- Action buttons size to their labels and align to the start instead of stretching
  across a column. Native selects use a centered chevron with a deliberate right
  inset and reserved text space.
- Render status rows only when they contain a message.
- Keep panel geometry stable as labels, status messages, and content change.
- Allow long names and values to wrap where useful. If truncating, provide a
  way to access the full value.
- At narrow widths, preserve access to navigation and the primary task. Decide
  the layout from content pressure and verify it in the browser.

### Edges and depth

All visible borders use the shared 1px `borderWidth` token in `Style`.
`outdent` uses lighter top/left and darker bottom/right borders. `indent`
reverses them. Primary controls use the yellow `importantOutdent` and
`importantIndent` variants. Buttons switch to inset edges while pressed.

The sidebar is flush with the viewport on its top, left, and bottom, so only
its right edge retains the bevel. Its icon-only toggle is a fixed 2rem square
with a centered glyph in both expanded and collapsed states.

Guideline: keep edges square and crisp in this theme. Add depth where it explains
structure or interaction, and avoid surrounding every text group with a panel.

## Components and behavior

| Pattern | Existing implementation | Guidance |
| --- | --- | --- |
| Primary action | [View.Button](src/View/Button.elm), `primary` | Use for the main action in a task area; label with a concrete verb. |
| Secondary action | [View.Button](src/View/Button.elm), `secondary` | Use for supporting actions with neutral gray treatment. |
| Text input | [View.TextField](src/View/TextField.elm) | Use the shared inset field and an associated visible label. |
| Longer input | [View.Textarea](src/View/Textarea.elm) | Reuse the shared component; size it for the expected content. |
| Selection | [View.Dropdown](src/View/Dropdown.elm) | Reuse after checking that its keyboard and labeling behavior fits the task. |
| Navigation | [Sidebar](src/Sidebar.elm), [Route](src/Route.elm) | Use anchors with `Route.href`; retain normal browser link behavior. |
| Dialog foundation | [View.Dialog](src/View/Dialog.elm) | Currently a positioned container; complete modal behavior before using it as a modal. |

Established: all navigation links use `S.link`: light blue with a persistent
underline, brighter hover/focus text, and a visible keyboard focus outline.
Apply the shared helper to sidebar, list, inline, and back-navigation anchors.
Sidebar navigation items are text-only links. Collapsing the sidebar hides the
whole navigation list, leaving its toggle available. Decide expanded or collapsed
content at the sidebar level; individual links do not depend on open state.
The sidebar toggle is a button because it changes interface state.
AllPersons lists names alphabetically as text links to person pages, with long
names wrapping inside the panel. Loading, empty, and failed requests have distinct
states; the empty state links to person creation and failures offer a retry.
Temporary development fixtures use one backend request alongside normal page
initialization; the backend remembers whether the data has already been filled. They do not introduce a setup page or block navigation.
After creating a person, show a success message with a `Route.href` link to the
saved person so the user can choose when to open it.

Person pages follow the human/AI distinction in the data model. `Main` chooses
independent `HumanPersonPage` and `AiPersonPage` modules when the person loads. Each owns its
model, message type, and update logic; common person details use a stateless view.
Human pages
show common identity details and an editable name; AI pages own the Identity, Aspirations,
goals, and memory editors. Do not render disabled or empty AI fields for humans.
Human and AI profile save feedback uses custom status types; AI goals and memories
have a separate `MindStatus` type. Derive display text from these states.
The human profile saving state also disables the form.
Main loads AI goals and memories before initializing AiPersonPage. Required lists
use plain List values, with [] representing a successful empty result. Initial
failures use the shared route failure view; later refresh failures retain the
last loaded lists and show feedback within the page.

AI goals and memories share the existing panel surface. A borderless native
fieldset disables related controls while saving. Keep human-entered records and
AI reflections visibly attributed; retired memories remain inspectable with an
explicit status. Clear only the successfully submitted draft and preserve other
drafts. Goals can be completed, abandoned, and reopened.

AllConversations owns the conversation list and creation form. ConversationPage
owns one conversation and its controls, with an All conversations link back to
the list. Keep list and detail page state separate; refresh only the open detail
page, and ignore responses originating from a different conversation. Fetch and
store the detail conversation by ID instead of loading and filtering the full list.
The route owns initial loading, failure, and not-found states; initialize the
detail page only with an existing conversation, so its model requires that value.
Likewise, load the conversation list and people in Main before initializing
AllConversations. Its lists are required values; an empty list means a successful
empty result. Main owns initial loading and failure states. All route loads share one loading
view and one failure view, without retry controls. Loading state retains the
resource ID, or the pending `Maybe Route` during development-data initialization.

Conversation's Person menu defaults to the current local human and displays
actual profile names for every participant. The human selection shows the message
composer and Send; an AI selection shows the reply request action. Switching
people preserves an unsent human draft. Sending a human message does not itself
request a reply and remains available during an AI turn. Automatic runs select
only AI participants. Offer “Run 1 turn” alongside “Run 6 turns” so the
conversation can advance a single automatic reply at a time. Human profiles have editable names used in transcripts and
future prompts, with an Edit human profile link in conversations. Development fixtures create the local
human as Chadtech. Missing accounts or unresolved person references are errors;
never substitute placeholder names. Resolve authors before accepting conversation
or note-history data, and before assembling AI prompts. Avoid viewer-relative
participant names such as You or Myself. Saved prompt snapshots retain their original text.

Conversation speaker validation appears beside the composer actions and names the
Person menu needed to recover. Associate feedback with that selector, announce
it as an alert, preserve the draft, and clear it when the selection changes.

Conversation autonomy is opt-in. Show its current interval and paid-call behavior
beside the controls. Stop cancels the active turn and disables autonomy; the
resulting state remains visible after page reload.

Note history loads on request and uses disclosure controls for earlier revisions.
Label each revision with its AI author and originating turn, keeping old content
on the existing panel surface. Prompt snapshots also load on request inside a
turn disclosure, so routine status refreshes do not reload large inspection data.
Render prompt inputs as attributed text with their selection reasons. Keep
excluded inputs and the exact serialized request in separate disclosures, so
the readable view preserves access to the original saved evidence.

Guidelines for new or revised components:

- Provide a visible keyboard focus indicator; removing an outline requires a
  clear replacement. Hover styling alone does not cover keyboard interaction.
- Give icon-only controls accessible names. Indicate the current route and
  expanded/collapsed state when applicable.
- Preserve entered values after a failed submission. Show pending status and
  prevent duplicate submissions when they would create duplicate work.
- Distinguish loading from an empty result. Empty states should explain what
  belongs there and offer the relevant next action.
- For a modal, provide an accessible name, focus entry and containment, an
  appropriate dismissal mechanism, and focus return to the invoking control.
- Use plain product language: for example, “Saving person…” for a save in
  progress. Mention backend details only when they help the user act.

## Working agreement for frontend changes

Before building, read this guide and the relevant shared component. Follow the
established direction and use the guidelines to resolve ordinary choices.
Keep reusable tokens in `src/Style.elm`, shared controls in `src/View/`, and page
composition in the page modules. Preserve unrelated work.

After changing a UI, check the actual task in the browser at a normal and a
narrow width, including keyboard operation, long content, and relevant states.
Run the build/format checks appropriate to the code change. Report compiler
validation and visual validation separately. Documentation-only changes need
link and content checks, not a frontend rebuild.

When feedback reveals a reusable lesson, update this document alongside the
relevant implementation. Record what changed and why. Do not turn an unreviewed
experiment into an established preference or expand a small task into a redesign.

## Open questions and known gaps

- Confirm the narrow-screen sidebar behavior through actual use.
- Choose and implement a consistent focus treatment across shared controls;
  the current text field removes its outline and only changes text color.
- Establish shared pending, disabled, and validation APIs where needed.
- Add current-route and expanded-state semantics to navigation where missing.
- Audit contrast, zoom, and keyboard behavior; the current palette is not certified.
- Complete dialog accessibility if a modal workflow is introduced.
- Add a small component showcase when visual comparison becomes useful.

## Decision log

| Date | Status | Decision and reason |
| --- | --- | --- |
| 2026-09-27 | Established | Initialize AI person pages only after goals and memories load successfully; keep initial loading and failure in Main and preserve loaded lists on refresh failure. |
| 2026-09-27 | Established | Offer single-turn and six-turn automatic runs together, allowing incremental conversation progress. |
| 2026-09-27 | Established | AI identity and goals/memory feedback use custom status types instead of strings, with text derived in the view. |
| 2026-09-27 | Established | Missing person references fail at data boundaries instead of displaying placeholder names. Development fixtures explicitly seed Chadtech as the local human. |
| 2026-09-27 | Established | Human profile save states use a custom type to keep form availability and feedback consistent. Profile layout uses shared Style helpers. |
| 2026-09-27 | Established | Name both person forms explicitly: `HumanPerson` and `AiPerson`, including their page modules and message wrappers. |
| 2026-09-27 | Established | Human and AI profiles are separate pages selected by Main, with independent models and messages. Share common presentation without sharing page behavior. |
| 2026-09-27 | Established | Human names are editable on their profiles and used consistently in speaker menus, transcripts, and future prompts; replace viewer-relative naming with participant identity. |
| 2026-09-26 | Established | Conversation pages use shared `Style` utilities for border resets, list markers, and text wrapping instead of inline CSS primitives. |
| 2026-09-26 | Established | Human profiles have only common person details. AI-only profile fields and editors belong exclusively to the AI variant, rather than using empty fields for humans. |
| 2026-09-26 | Superseded | Add Myself as the default conversation speaker. Separate human Send from AI reply requests, retain drafts across selection changes, and allow human posting during generation. |
| 2026-09-26 | Established | Show missing or ineligible reply-person feedback beside Send, with an accessible alert and selector association; preserve the draft and clear feedback on selection changes. Errors below history were easy to miss. |
| 2026-09-13 | Established | Main shares loading and failure views and removes route retry controls. Development-data loading retains the pending route and follows the latest navigation after initialization. |
| 2026-09-13 | Established | Main loads AllConversations flags before initialization, keeping required lists in the page model and initial loading and retry in the route. |
| 2026-09-12 | Established | Conversation detail fetches and stores only its own conversation by ID; the full list belongs to AllConversations. Load before initializing the detail page so its conversation is required. |
| 2026-09-07 | Established | Removed sidebar navigation icons following user feedback. Render the navigation list only in the expanded sidebar and keep the open-state decision at the sidebar level. |
| 2026-09-07 | Established | Give all links a shared light-blue, underlined treatment with brighter hover/focus and a focus outline, following feedback that links were hard to recognize. |
| 2026-09-07 | Established | Use available page width, content-sized action buttons, consistent form gaps, and an inset select chevron following screenshot feedback. |
| 2026-09-07 | Established | Split conversation browsing and detail into AllConversations and ConversationPage, keeping creation with the list and existing URLs intact. |
| 2026-09-07 | Established | Changed all borders from 2px to 1px following user feedback. Shared bevels and border helpers use one width token, including pressed controls. |
| 2026-09-07 | Established | Use `S.flexWrap` and Tailwind-inspired names for shared layout utilities. Keep page subsections in their owning Elm page. |
| 2026-09-05 | Established baseline | Recorded the existing People theme: nightwood, Fira Code with local fallbacks, beveled controls, and link-based navigation. Grounds future work in the current frontend. |
| 2026-09-06 | Experiment | Shared-note revisions load on request and expand individually, with author and turn attribution. Keeps earlier AI contributions inspectable without expanding every note. |
| 2026-09-06 | Experiment | Conversation autonomy shows explicit on/off state, interval, and call cost behavior. Browser-verified enable, reload, and Stop. |
| 2026-09-06 | Experiment | Person goals and memories use attributed records and a borderless disabled fieldset for pending edits. Browser-verified creation, completion, and keyboard retirement; narrow use currently requires collapsing the sidebar. |
| 2026-09-05 | Established | All text uses normal weight; removed bold labels and headings and reset browser default bold styling following user feedback. |
| 2026-09-05 | Established | All text uses one size; headers are darker. Removed the type scale and muted-text role following user feedback. |
| 2026-09-05 | Guideline | Added reusable layout, interaction, and verification guidance, with unresolved implementation gaps listed separately. Provides a starting point for iteration. |
| 2026-09-05 | Established | Successful person creation shows a link to the saved person, providing an explicit next step without automatic navigation. |
| 2026-09-05 | Established | Keep page minimum height explicit with shared `S.minHFullViewport`; user feedback rejected styling page children from `Main`. |
| 2026-09-05 | Established | Keep the sidebar bevel only on its exposed right edge and make its toggle square, following user feedback. |
| 2026-09-05 | Established | Reset margin and padding to zero globally; spacing is explicit, following user feedback. |
| 2026-09-05 | Established | AllPersons uses a compact alphabetical list of person links, with wrapping names and distinct loading, empty, and retry states. Reuses the page panel and navigation conventions. |
| 2026-09-05 | Established | Person and NewPerson use one panel inset and gaps between content groups; removed stacked heading, wrapper, and row padding after user feedback about confusing spacing. |

| 2026-09-05 | Experiment | Development fixtures use one idempotent backend request from Main.init, with no dedicated page or startup gate. The backend remembers completion to prevent duplicates on reload. |

For future entries: `Date | Established / Guideline / Experiment / Superseded |
Decision, reason, and relevant component or evidence`. Update the main section
when superseding a decision. Keep experiments scoped and record what would make
us keep, revise, or remove them.
