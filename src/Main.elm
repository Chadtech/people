module Main exposing (main)

import AllConversations
import AllPersons
import Browser
import Browser.Navigation as Navigation
import ConversationId exposing (ConversationId)
import ConversationPage
import Css.Global
import DevelopmentData
import Document exposing (Document)
import Effect as E exposing (Eff)
import Html.Styled as H
import Html.Styled.Attributes as A
import NewPerson
import Person exposing (PersonPageFlags)
import PersonId exposing (PersonId)
import PersonPage
import Remote exposing (Remote)
import Route exposing (Route)
import Shared
import Sidebar
import Style as S
import Url exposing (Url)


type Page
    = AllConversations AllConversations.Model
    | Conversation ConversationPage.Model
    | AllPersons AllPersons.Model
    | NewPerson NewPerson.Model
    | Person PersonPage.Model
    | LoadingPerson Shared.Model PersonId
    | PersonLoadFailed Shared.Model
    | PageNotFound Shared.Model
    | LoadingDevData Shared.Model
    | FailedToLoadDevData Shared.Model


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangesRoute (Maybe Route)
    | AllPersonsMsg AllPersons.Msg
    | NewPersonMsg NewPerson.Msg
    | PersonMsg PersonPage.Msg
    | AllConversationsMsg AllConversations.Msg
    | ConversationMsg ConversationId ConversationPage.Msg
    | LoadedPersonPage PersonId (Remote PersonPageFlags)
    | SidebarMsg Sidebar.Msg
    | DevelopmentDataResponseReceived (Maybe Route) (Maybe ())


main : Program () Page Msg
main =
    Browser.application
        { init =
            \() url key ->
                init url key
                    |> Tuple.mapSecond (E.toCmd key)
        , onUrlChange = ChangesRoute << Route.fromUrl
        , onUrlRequest = ClickedLink
        , update =
            \msg page ->
                update msg page
                    |> Tuple.mapSecond (E.toCmd (getShared page).key)
        , subscriptions = subscriptions
        , view = view
        }


init : Url -> Navigation.Key -> ( Page, Eff Msg )
init url key =
    let
        route : Maybe Route
        route =
            Route.fromUrl url
    in
    ( LoadingDevData (Shared.init key)
    , DevelopmentData.init (DevelopmentDataResponseReceived route)
    )


getShared : Page -> Shared.Model
getShared page =
    case page of
        AllConversations model ->
            AllConversations.shared model

        Conversation model ->
            ConversationPage.shared model

        AllPersons allPersonsModel ->
            AllPersons.shared allPersonsModel

        NewPerson newPersonModel ->
            NewPerson.shared newPersonModel

        Person personModel ->
            PersonPage.shared personModel

        LoadingPerson sharedModel _ ->
            sharedModel

        PersonLoadFailed sharedModel ->
            sharedModel

        PageNotFound sharedModel ->
            sharedModel

        LoadingDevData shared ->
            shared

        FailedToLoadDevData shared ->
            shared


setShared : Shared.Model -> Page -> Page
setShared sharedModel page =
    case page of
        AllConversations model ->
            AllConversations (AllConversations.setShared sharedModel model)

        Conversation model ->
            Conversation (ConversationPage.setShared sharedModel model)

        AllPersons allPersonsModel ->
            AllPersons (AllPersons.setShared sharedModel allPersonsModel)

        NewPerson newPersonModel ->
            NewPerson (NewPerson.setShared sharedModel newPersonModel)

        Person personModel ->
            Person (PersonPage.setShared sharedModel personModel)

        LoadingPerson _ personId ->
            LoadingPerson sharedModel personId

        PersonLoadFailed _ ->
            PersonLoadFailed sharedModel

        PageNotFound _ ->
            PageNotFound sharedModel

        LoadingDevData _ ->
            LoadingDevData sharedModel

        FailedToLoadDevData _ ->
            FailedToLoadDevData sharedModel


handleRouteChange : Maybe Route -> Shared.Model -> ( Page, Eff Msg )
handleRouteChange maybeRoute sharedModel =
    case maybeRoute of
        Just Route.Conversations ->
            AllConversations.init sharedModel
                |> Tuple.mapFirst AllConversations
                |> Tuple.mapSecond (E.map AllConversationsMsg)

        Just (Route.Conversation id) ->
            ConversationPage.init sharedModel id
                |> Tuple.mapFirst Conversation
                |> Tuple.mapSecond (E.map (ConversationMsg id))

        Just Route.AllPersons ->
            AllPersons.init sharedModel
                |> Tuple.mapFirst AllPersons
                |> Tuple.mapSecond (E.map AllPersonsMsg)

        Just Route.NewPerson ->
            NewPerson.init sharedModel
                |> NewPerson
                |> E.withOut

        Just (Route.Person personId) ->
            ( LoadingPerson sharedModel personId
            , E.fetch (LoadedPersonPage personId) (Person.loadPersonPage personId)
            )

        Nothing ->
            PageNotFound sharedModel
                |> E.withOut


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

        DevelopmentDataResponseReceived maybeRoute result ->
            case result of
                Just () ->
                    handleRouteChange maybeRoute (getShared page)

                Nothing ->
                    ( page, E.none )

        SidebarMsg sidebarMsg ->
            case sidebarMsg of
                Sidebar.OpenToggleClicked ->
                    ( setShared (Shared.toggleSidebar (getShared page)) page
                    , E.none
                    )

        AllPersonsMsg allPersonsMsg ->
            case page of
                AllPersons allPersonsModel ->
                    AllPersons.update allPersonsMsg allPersonsModel
                        |> Tuple.mapFirst AllPersons
                        |> Tuple.mapSecond (E.map AllPersonsMsg)

                _ ->
                    ( page, E.none )

        NewPersonMsg newPersonMsg ->
            case page of
                NewPerson newPersonModel ->
                    NewPerson.update newPersonMsg newPersonModel
                        |> Tuple.mapFirst NewPerson
                        |> Tuple.mapSecond (E.map NewPersonMsg)

                _ ->
                    ( page, E.none )

        AllConversationsMsg allConversationsMsg ->
            case page of
                AllConversations model ->
                    AllConversations.update allConversationsMsg model
                        |> Tuple.mapFirst AllConversations
                        |> Tuple.mapSecond (E.map AllConversationsMsg)

                _ ->
                    ( page, E.none )

        ConversationMsg id conversationMsg ->
            case page of
                Conversation model ->
                    if model.conversationId == id then
                        ConversationPage.update conversationMsg model
                            |> Tuple.mapFirst Conversation
                            |> Tuple.mapSecond (E.map (ConversationMsg id))

                    else
                        ( page, E.none )

                _ ->
                    ( page, E.none )

        PersonMsg personMsg ->
            case page of
                Person personModel ->
                    PersonPage.update personMsg personModel
                        |> Tuple.mapFirst Person
                        |> Tuple.mapSecond (E.map PersonMsg)

                _ ->
                    ( page, E.none )

        LoadedPersonPage personId maybePersonPageFlags ->
            case page of
                LoadingPerson sharedModel requestedId ->
                    if personId /= requestedId then
                        ( page, E.none )

                    else
                        case maybePersonPageFlags of
                            Remote.Found flags ->
                                PersonPage.init sharedModel flags
                                    |> Tuple.mapFirst Person
                                    |> Tuple.mapSecond (E.map PersonMsg)

                            Remote.Failed ->
                                ( PersonLoadFailed sharedModel, E.none )

                            Remote.NotFound ->
                                ( PageNotFound sharedModel, E.none )

                _ ->
                    ( page, E.none )


view : Page -> Browser.Document Msg
view page =
    let
        document : Document Msg
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
        AllConversations model ->
            AllConversations.view model |> Document.map AllConversationsMsg

        Conversation model ->
            ConversationPage.view model |> Document.map (ConversationMsg model.conversationId)

        AllPersons allPersonsModel ->
            AllPersons.view allPersonsModel
                |> Document.map AllPersonsMsg

        NewPerson newPersonModel ->
            NewPerson.document newPersonModel
                |> Document.map NewPersonMsg

        Person personModel ->
            PersonPage.document personModel
                |> Document.map PersonMsg

        LoadingPerson _ _ ->
            { title = "Loading person"
            , body =
                [ H.p
                    [ A.css [ S.p4 ], A.attribute "role" "status" ]
                    [ H.text
                        "Loading person…"
                    ]
                ]
            }

        PersonLoadFailed _ ->
            { title = "Could not load person"
            , body = [ H.p [] [ H.text "Could not load this person. Reload to retry." ] ]
            }

        PageNotFound _ ->
            { title = "Page not found"
            , body =
                [ H.main_ []
                    [ H.h1 [] [ H.text "Page not found" ]
                    ]
                ]
            }

        LoadingDevData _ ->
            { title = "Loading development data"
            , body =
                [ H.p
                    [ A.css [ S.p4 ], A.attribute "role" "status" ]
                    [ H.text
                        "Loading development data…"
                    ]
                ]
            }

        FailedToLoadDevData _ ->
            { title = "Could not load development data"
            , body =
                [ H.p
                    [ A.css [ S.p4 ], A.attribute "role" "alert" ]
                    [ H.text
                        "Could not load development data. Reload to retry."
                    ]
                ]
            }


subscriptions : Page -> Sub Msg
subscriptions page =
    case page of
        Conversation model ->
            ConversationPage.subscriptions
                |> Sub.map (ConversationMsg model.conversationId)

        _ ->
            Sub.none
