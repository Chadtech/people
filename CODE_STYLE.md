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
