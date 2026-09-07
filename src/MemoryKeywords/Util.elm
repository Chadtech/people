module MemoryKeywords.Util exposing (toString)

import MemoryKeywords exposing (MemoryKeywords(..))


toString : MemoryKeywords -> String
toString (MemoryKeywords value) =
    value
