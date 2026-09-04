module Shared exposing (Model, init, toggleSidebar)

import Browser.Navigation as Navigation


type alias Model =
    { key : Navigation.Key
    , sidebarOpen : Bool
    }


init : Navigation.Key -> Model
init key =
    { key = key
    , sidebarOpen = True
    }


toggleSidebar : Model -> Model
toggleSidebar model =
    { model | sidebarOpen = not model.sidebarOpen }
