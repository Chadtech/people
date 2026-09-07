module ConversationTitle.Util exposing (toString)

import ConversationTitle exposing (ConversationTitle(..))


toString : ConversationTitle -> String
toString (ConversationTitle value) =
    value
