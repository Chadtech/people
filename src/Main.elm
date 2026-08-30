module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Html
import NewPerson
import Route exposing (Route)
import Shared
import Url exposing (Url)


type Model
    = NewPerson NewPerson.Model
    | PageNotFound Shared.Model


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangesRoute (Maybe Route)
    | NewPersonMsg NewPerson.Msg


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangesRoute << Route.fromUrl
        , onUrlRequest = ClickedLink
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    changeRoute (Route.fromUrl url) (Shared.init key)


changeRoute : Maybe Route -> Shared.Model -> ( Model, Cmd Msg )
changeRoute maybeRoute sharedModel =
    case maybeRoute of
        Just Route.NewPerson ->
            NewPerson.init sharedModel
                |> Tuple.mapFirst NewPerson
                |> Tuple.mapSecond (Cmd.map NewPersonMsg)

        Nothing ->
            ( PageNotFound sharedModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl (shared model).key (Url.toString url) )

                Browser.External url ->
                    ( model, Navigation.load url )

        ChangesRoute maybeRoute ->
            changeRoute maybeRoute (shared model)

        NewPersonMsg newPersonMsg ->
            case model of
                NewPerson newPersonModel ->
                    NewPerson.update newPersonMsg newPersonModel
                        |> Tuple.mapFirst NewPerson
                        |> Tuple.mapSecond (Cmd.map NewPersonMsg)

                PageNotFound _ ->
                    ( model, Cmd.none )


shared : Model -> Shared.Model
shared model =
    case model of
        NewPerson newPersonModel ->
            NewPerson.shared newPersonModel

        PageNotFound sharedModel ->
            sharedModel


view : Model -> Browser.Document Msg
view model =
    case model of
        NewPerson newPersonModel ->
            NewPerson.view newPersonModel
                |> mapDocument NewPersonMsg

        PageNotFound _ ->
            { title = "Page not found"
            , body =
                [ Html.main_ []
                    [ Html.h1 [] [ Html.text "Page not found" ]
                    ]
                ]
            }


mapDocument : (a -> msg) -> Browser.Document a -> Browser.Document msg
mapDocument toMsg document =
    { title = document.title
    , body = List.map (Html.map toMsg) document.body
    }
