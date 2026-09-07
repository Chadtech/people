module GoalDescription.Util exposing (toString)

import GoalDescription exposing (GoalDescription(..))


toString : GoalDescription -> String
toString (GoalDescription value) =
    value
