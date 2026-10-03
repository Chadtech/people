# Vendored Elm collections

The complete upstream modules are copied into `src` so they are available without
adding package dependencies to `elm.json`. Keep these copies unchanged when
updating them; project code follows `CODE_STYLE.md`.

| Local source | Package version | Upstream source | License |
| --- | --- | --- | --- |
| `src/AssocList.elm` | `pzp1997/assoc-list` 1.0.0 | https://github.com/pzp1997/assoc-list/blob/1.0.0/src/AssocList.elm | [BSD-3-Clause](assoc-list/LICENSE) |
| `src/AssocSet.elm` | `erlandsona/assoc-set` 1.1.3 | https://github.com/erlandsona/assoc-set/blob/1.1.3/src/AssocSet.elm | [BSD-3-Clause](assoc-set/LICENSE) |

Import them as `AssocList as Dict exposing (Dict)` and
`AssocSet as Set exposing (Set)` to use domain types directly as keys and members.
They use equality rather than ordering, so values containing functions cannot be
used as keys or members. Collection operations generally take linear time.
