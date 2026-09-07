module MessageId.Util exposing (fromString, toString)

import Acadia.UInt64 as UInt64
import MessageId exposing (MessageId(..))


toString : MessageId -> String
toString (MessageId value) =
    UInt64.toString value


fromString : String -> Maybe MessageId
fromString value =
    value
        |> UInt64.fromString
        |> Maybe.map MessageId
