module ConversationId.Util exposing (fromString, toString)

import Acadia.UInt64 as UInt64
import ConversationId exposing (ConversationId(..))


toString : ConversationId -> String
toString (ConversationId value) =
    UInt64.toString value


fromString : String -> Maybe ConversationId
fromString value =
    value
        |> UInt64.fromString
        |> Maybe.map ConversationId
