module GenerationError.Util exposing (toString)

import GenerationError exposing (GenerationError(..))


toString : GenerationError -> String
toString (GenerationError value) =
    value
