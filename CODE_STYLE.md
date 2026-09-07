# Code style

## Elm imports

Use these aliases consistently:

| Module | Alias |
| --- | --- |
| `Style` | `S` |
| `Html.Styled` (or `Html`) | `H` |
| `Html.Styled.Attributes` (or `Html.Attributes`) | `A` |
| `Html.Styled.Events` (or `Html.Events`) | `Ev` |
| `Effect` | `E` |

Types with clear names may be exposed and used unqualified, such as `Html`,
`Attribute`, and `Eff`. The HTML attribute type is singular: `Attribute`;
`Attributes` is the module name.

Always qualify imported types with ambiguous names, such as `Shared.Model` or
`Sidebar.Msg`. Types defined in the current module can use their local names.

Do not expose functions in imports. Call imported functions through their module
name or alias, such as `H.div`, `A.class`, and `Ev.onClick`.

For example:

```elm
import Effect as E exposing (Eff)
import Html.Styled as H exposing (Attribute, Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as Ev
import Shared
import Sidebar
import Style as S
```

Use explicit type names in `exposing` lists; do not use `exposing (..)` on imports.

## Messages describe events

`Msg` constructor names must be past-tense references to what happened in the
real world: a user interaction, a response arriving, or another external event.
Name the actual event precisely, including its source when needed.

Keep a strong separation between **what happened** and **what we do about it**:

- Messages capture events and their relevant data.
- The update function connects those events to the corresponding functionality.
- Functions implement that functionality and are named for what they do.

For example:

| What happened (`Msg`) | What we do about it (function) |
| --- | --- |
| `SaveButtonClicked` | `savePerson` |
| `NameInputChanged String` | `setName` |
| `SidebarToggleClicked` | `toggleSidebar` |
| `PeopleResponseReceived (Result Http.Error (List Person))` | `handlePeopleResponse` |

Do not name messages as commands such as `SavePerson`, `SetName`, or
`ToggleSidebar`. Adding past tense to an intended action is not enough:
`PersonSaved` describes a confirmed save, so it must not represent a click that
merely requests one. The event name should remain accurate even if the response
to that event changes.


## Layout and type signatures

Prefer short lines (roughly 80 columns) and multiline function calls, records,
lists, and type signatures. Give every function and named value a type signature,
including local helpers. Generated bindings follow the generator's format.
Long literal strings in Acadia are an exception: the current runtime does not
support string concatenation.

Keep Haskell simple and explicit, with ordinary data types, pattern matching,
and small functions in an Elm-like style. Use explicit imports so names such as
`Data.Aeson.Value` have an obvious source. `fourmolu.yaml` records the formatting
settings for handwritten Haskell; do not format generated bindings by hand.

## Domain types and module boundaries

Use distinct custom types for every domain ID, following `PersonId`; never use
raw integers or aliases for IDs. Wrap domain values that can be confused at an
API boundary, such as message content, notes, revisions, and time intervals.
Keep conversions at input, display, serialization, and parsing boundaries.
Give each domain value its own module, with related helpers beside its type.
Use `Maybe String` for optional page errors instead of an empty-string sentinel.

Database modules own independent concepts: `Person`, `Goal`, and `Memory`.
Shared provenance lives in `Origin`, so goals and memories need not depend on
each other. `PersonPage.elm` owns its goals and memories section directly; avoid
separate model/update/message modules for ordinary page subsections. Keep one
`Msg` type and attach the originating ID to asynchronous responses that may
arrive after navigation.

Use `Remote data = Failed | NotFound | Found data` for optional remote resources.
`Effect.fetch` translates the transport failure and optional result directly,
without nested `Maybe` values in page messages. Loading remains page state.
