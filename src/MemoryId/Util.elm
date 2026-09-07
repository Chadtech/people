module MemoryId.Util exposing (fromString, toString)

import Acadia.UInt64 as UInt64
import MemoryId exposing (MemoryId(..))


toString : MemoryId -> String
toString (MemoryId value) =
    UInt64.toString value


fromString : String -> Maybe MemoryId
fromString value =
    value
        |> UInt64.fromString
        |> Maybe.map MemoryId
