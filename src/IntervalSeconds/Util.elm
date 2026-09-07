module IntervalSeconds.Util exposing (toString)

import Acadia.UInt64 as UInt64
import IntervalSeconds exposing (IntervalSeconds(..))


toString : IntervalSeconds -> String
toString (IntervalSeconds value) =
    UInt64.toString value
