module PromptSnapshot.Util exposing (toString)

import PromptSnapshot exposing (PromptSnapshot(..))


toString : PromptSnapshot -> String
toString (PromptSnapshot value) =
    value
