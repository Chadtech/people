module MessageContent.Util exposing (toString)

import MessageContent exposing (MessageContent(..))


toString : MessageContent -> String
toString (MessageContent value) =
    value
