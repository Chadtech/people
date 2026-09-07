module GoalId.Util exposing (fromString, toString)

import Acadia.UInt64 as UInt64
import GoalId exposing (GoalId(..))


toString : GoalId -> String
toString (GoalId value) =
    UInt64.toString value


fromString : String -> Maybe GoalId
fromString value =
    value
        |> UInt64.fromString
        |> Maybe.map GoalId
