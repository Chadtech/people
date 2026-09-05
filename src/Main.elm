module Main exposing (main)

import Backend exposing (PersonPageFlags)
import Browser
import Browser.Navigation as Navigation
import Css.Global
import Document exposing (Document)
import Effect as E exposing (Eff)
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
        { init =
            \flags url key ->
                init flags url key
                    |> Tuple.mapSecond (E.toCmd key)
        , onUrlChange = ChangesRoute << Route.fromUrl
        , onUrlRequest = ClickedLink
        , update =
            \msg page ->
                update msg page
                    |> Tuple.mapSecond (E.toCmd (getShared page).key)
        , subscriptions = always Sub.none
        , view = view
        }


init : () -> Url -> Navigation.Key -> ( Page, Eff Msg )
init _ url key =
    handleRouteChange (Route.fromUrl url) (Shared.init key)


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


handleRouteChange : Maybe Route -> Shared.Model -> ( Page, Eff Msg )
handleRouteChange maybeRoute sharedModel =
    case maybeRoute of
        Just Route.NewPerson ->
            ( NewPerson <| NewPerson.init sharedModel
            , E.none
            )

        Just (Route.Person personId) ->
            ( PageNotFound sharedModel
            , E.attempt LoadedPersonPage (Backend.loadPersonPage personId)
            )

        Nothing ->
            ( PageNotFound sharedModel, E.none )


update : Msg -> Page -> ( Page, Eff Msg )
update msg page =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( page, E.pushUrl (Url.toString url) )

                Browser.External url ->
                    ( page, E.load url )

        ChangesRoute maybeRoute ->
            handleRouteChange maybeRoute (getShared page)

        SidebarMsg sidebarMsg ->
            case sidebarMsg of
                Sidebar.OpenToggleClicked ->
                    ( setShared (Shared.toggleSidebar (getShared page)) page
                    , E.none
                    )

        NewPersonMsg newPersonMsg ->
            case page of
                NewPerson newPersonModel ->
                    NewPerson.update newPersonMsg newPersonModel
                        |> Tuple.mapFirst NewPerson
                        |> Tuple.mapSecond (E.map NewPersonMsg)

                _ ->
                    ( page, E.none )

        LoadedPersonPage maybePersonPageFlags ->
            case maybePersonPageFlags of
                Just (Just flags) ->
                    ( Person <| Person.init (getShared page) flags
                    , E.none
                    )

                _ ->
                    ( PageNotFound (getShared page)
                    , E.none
                    )


view : Page -> Browser.Document Msg
view page =
    let
        document =
            pageDocument page
    in
    { title = document.title
    , body =
        [ Css.Global.global S.global
        , H.div
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
                document.body
            ]
        ]
    }
        |> Document.toBrowserDocument


pageDocument : Page -> Document Msg
pageDocument page =
    case page of
        NewPerson newPersonModel ->
            NewPerson.document newPersonModel
                |> Document.map NewPersonMsg

        Person personModel ->
            Person.view personModel

        PageNotFound _ ->
            { title = "Page not found"
            , body =
                [ H.main_ []
                    [ H.h1 [] [ H.text "Page not found" ]
                    ]
                ]
            }
