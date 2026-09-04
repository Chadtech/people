module Person exposing
    ( Model
    , Msg
    , init
    , setShared
    , shared
    , update
    , view
    )

import Backend exposing (Person, PersonPageFlags)
import Browser exposing (Document)
import Html.Styled as H
import Shared


type alias Model =
    { shared : Shared.Model
    , person : Person
    }


type Msg
    = NoMsgYet


init : Shared.Model -> PersonPageFlags -> Model
init sharedModel flags =
    { shared = sharedModel
    , person = flags.person
    }


shared : Model -> Shared.Model
shared model =
    model.shared


setShared : Shared.Model -> Model -> Model
setShared sharedModel model =
    { model | shared = sharedModel }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoMsgYet ->
            ( model, Cmd.none )


view : Model -> Document msg
view model =
    { title = "Person"
    , body =
        [ H.text "WIP"
            |> H.toUnstyled
        ]
    }
