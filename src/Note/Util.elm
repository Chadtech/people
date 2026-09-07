module Note.Util exposing (toString)

import Note exposing (Note(..))


toString : Note -> String
toString (Note value) =
    value
