module Shared exposing (Model, init)

import Browser.Navigation as Navigation


type alias Model =
    { key : Navigation.Key
    }


init : Navigation.Key -> Model
init key =
    { key = key
    }
