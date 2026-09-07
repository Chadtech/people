module GenerationId.Util exposing (fromString, toString)

import Acadia.UInt64 as UInt64
import GenerationId exposing (GenerationId(..))


toString : GenerationId -> String
toString (GenerationId value) =
    UInt64.toString value


fromString : String -> Maybe GenerationId
fromString value =
    value
        |> UInt64.fromString
        |> Maybe.map GenerationId
