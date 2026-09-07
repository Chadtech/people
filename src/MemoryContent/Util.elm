module MemoryContent.Util exposing (toString)

import MemoryContent exposing (MemoryContent(..))


toString : MemoryContent -> String
toString (MemoryContent value) =
    value
