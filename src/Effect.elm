module Effect exposing
    ( Eff
    , attempt
    , batch
    , load
    , map
    , none
    , pushUrl
    , toCmd
    )

import Acadia.Transaction exposing (Transaction(..))
import Browser.Navigation as Navigation
import Bytes.Decode as Decode
import Route exposing (Route)


type Eff msg
    = None
    | Batch (List (Eff msg))
    | ApiRequest msg (Transaction msg)
    | PushUrl String
    | PushRoute Route
    | Load String


none : Eff msg
none =
    None


batch : List (Eff msg) -> Eff msg
batch =
    Batch


attempt : (Maybe a -> msg) -> Transaction a -> Eff msg
attempt toMsg (Transaction encoder decoder) =
    ApiRequest
        (toMsg Nothing)
        (Transaction encoder (Decode.map (toMsg << Just) decoder))


pushUrl : String -> Eff msg
pushUrl =
    PushUrl


pushRoute : Route -> Eff msg
pushRoute =
    PushRoute


load : String -> Eff msg
load =
    Load


map : (a -> b) -> Eff a -> Eff b
map toMsg effect =
    case effect of
        None ->
            None

        Batch effects ->
            Batch (List.map (map toMsg) effects)

        ApiRequest onFailure (Transaction encoder decoder) ->
            ApiRequest (toMsg onFailure)
                (Transaction encoder (Decode.map toMsg decoder))

        PushUrl url ->
            PushUrl url

        Load url ->
            Load url

        PushRoute route ->
            PushRoute route


toCmd : Navigation.Key -> Eff msg -> Cmd msg
toCmd key effect =
    case effect of
        None ->
            Cmd.none

        Batch effects ->
            Cmd.batch (List.map (toCmd key) effects)

        ApiRequest onFailure transaction ->
            Acadia.Transaction.attempt
                "/_endpoints"
                (Maybe.withDefault onFailure)
                transaction

        PushUrl url ->
            Navigation.pushUrl key url

        Load url ->
            Navigation.load url

        PushRoute route ->
            Navigation.pushUrl key (Route.toString route)
