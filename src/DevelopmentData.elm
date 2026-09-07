module DevelopmentData exposing (init)

import Effect as E exposing (Eff)
import Fixtures


init : (Maybe () -> msg) -> Eff msg
init toMsg =
    E.attempt toMsg Fixtures.fillDevelopmentData
