module Main exposing (main)

import Api
import Backend exposing (PersonPageFlags)
import Browser
import Browser.Navigation as Navigation
import Html.Styled as H
import Html.Styled.Attributes as A
import NewPerson
import Person
import Route exposing (Route)
import Shared
import Sidebar
import Style as S
import Url exposing (Url)


type Page
    = NewPerson NewPerson.Model
    | Person Person.Model
    | PageNotFound Shared.Model


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangesRoute (Maybe Route)
    | NewPersonMsg NewPerson.Msg
    | LoadedPersonPage (Maybe (Maybe PersonPageFlags))
    | SidebarMsg Sidebar.Msg


main : Program () Page Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangesRoute << Route.fromUrl
        , onUrlRequest = ClickedLink
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }


init : () -> Url -> Navigation.Key -> ( Page, Cmd Msg )
init _ url key =
    changeRoute (Route.fromUrl url) (Shared.init key)


getShared : Page -> Shared.Model
getShared page =
    case page of
        NewPerson newPersonModel ->
            NewPerson.shared newPersonModel

        Person personModel ->
            Person.shared personModel

        PageNotFound sharedModel ->
            sharedModel


setShared : Shared.Model -> Page -> Page
setShared sharedModel page =
    case page of
        NewPerson newPersonModel ->
            NewPerson (NewPerson.setShared sharedModel newPersonModel)

        Person personModel ->
            Person (Person.setShared sharedModel personModel)

        PageNotFound _ ->
            PageNotFound sharedModel


changeRoute : Maybe Route -> Shared.Model -> ( Page, Cmd Msg )
changeRoute maybeRoute sharedModel =
    case maybeRoute of
        Just Route.NewPerson ->
            NewPerson.init sharedModel
                |> Tuple.mapFirst NewPerson
                |> Tuple.mapSecond (Cmd.map NewPersonMsg)

        Just (Route.Person personId) ->
            ( PageNotFound sharedModel
            , Api.attempt LoadedPersonPage (Backend.loadPersonPage personId)
            )

        Nothing ->
            ( PageNotFound sharedModel, Cmd.none )


update : Msg -> Page -> ( Page, Cmd Msg )
update msg page =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( page, Navigation.pushUrl (getShared page).key (Url.toString url) )

                Browser.External url ->
                    ( page, Navigation.load url )

        ChangesRoute maybeRoute ->
            changeRoute maybeRoute (getShared page)

        SidebarMsg sidebarMsg ->
            case sidebarMsg of
                Sidebar.OpenToggleClicked ->
                    ( setShared (Shared.toggleSidebar (getShared page)) page
                    , Cmd.none
                    )

        NewPersonMsg newPersonMsg ->
            case page of
                NewPerson newPersonModel ->
                    NewPerson.update newPersonMsg newPersonModel
                        |> Tuple.mapFirst NewPerson
                        |> Tuple.mapSecond (Cmd.map NewPersonMsg)

                _ ->
                    ( page, Cmd.none )

        LoadedPersonPage maybePersonPageFlags ->
            case maybePersonPageFlags of
                Just (Just flags) ->
                    ( Person <| Person.init (getShared page) flags
                    , Cmd.none
                    )

                _ ->
                    ( PageNotFound (getShared page)
                    , Cmd.none
                    )


view : Page -> Browser.Document Msg
view page =
    let
        document =
            pageDocument page
    in
    { title = document.title
    , body =
        [ H.div
            [ A.css
                [ S.bgNightwoodGrain
                , S.fontMonospace
                , S.hFullViewport
                , S.row
                , S.textGray4
                , S.wFull
                ]
            ]
            [ Sidebar.view (getShared page)
                |> H.map SidebarMsg
            , H.main_
                [ A.css [ S.flex1, S.minW0, S.overflowAuto ] ]
                (List.map H.fromUnstyled document.body)
            ]
            |> H.toUnstyled
        ]
    }


pageDocument : Page -> Browser.Document Msg
pageDocument page =
    case page of
        NewPerson newPersonModel ->
            NewPerson.view newPersonModel
                |> mapDocument NewPersonMsg

        Person personModel ->
            Person.view personModel

        PageNotFound _ ->
            { title = "Page not found"
            , body =
                [ H.main_ []
                    [ H.h1 [] [ H.text "Page not found" ]
                    ]
                    |> H.toUnstyled
                ]
            }


mapDocument : (a -> msg) -> Browser.Document a -> Browser.Document msg
mapDocument toMsg document =
    { title = document.title
    , body =
        List.map
            (H.fromUnstyled >> H.map toMsg >> H.toUnstyled)
            document.body
    }
