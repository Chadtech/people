module PersonId.Util exposing (fromString, toString)

import Acadia.UInt64 as UInt64
import PersonId exposing (PersonId(..))


toString : PersonId -> String
toString (PersonId value) =
    UInt64.toString value


fromString : String -> Maybe PersonId
fromString value =
    value
        |> UInt64.fromString
        |> Maybe.map PersonId
