module Main exposing (main)

import AiPersonPage
import AiPersonProfile exposing (AiPersonProfile)
import AllConversations
import AllPersons
import Browser
import Browser.Navigation as Navigation
import Chat
import Conversation
import ConversationId exposing (ConversationId)
import ConversationPage
import Css.Global
import DevelopmentData
import Document exposing (Document)
import Effect as E exposing (Eff)
import Goal exposing (Goal)
import Html.Styled as H
import Html.Styled.Attributes as A
import HumanPersonPage
import Memory exposing (Memory)
import NewPerson
import Person exposing (Person, PersonPageFlags)
import PersonId exposing (PersonId)
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
    | HumanPerson HumanPersonPage.Model
    | AiPerson AiPersonPage.Model
    | Loading Shared.Model LoadTarget
    | LoadFailed Shared.Model
    | PageNotFound Shared.Model


type LoadTarget
    = ConversationsTarget
    | ConversationTarget ConversationId
    | PersonTarget PersonId
    | AiPersonGoalsTarget Person AiPersonProfile
    | AiPersonMemoriesTarget Person AiPersonProfile (List Goal)
    | DevelopmentDataTarget (Maybe Route)


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangesRoute (Maybe Route)
    | AllPersonsMsg AllPersons.Msg
    | NewPersonMsg NewPerson.Msg
    | HumanPersonMsg HumanPersonPage.Msg
    | AiPersonMsg AiPersonPage.Msg
    | AllConversationsMsg AllConversations.Msg
    | ConversationMsg ConversationId ConversationPage.Msg
    | AllConversationsResponseReceived (Maybe AllConversations.Flags)
    | ConversationResponseReceived ConversationId (Remote ConversationPage.Flags)
    | LoadedPersonPage PersonId (Remote PersonPageFlags)
    | AiPersonGoalsResponseReceived PersonId (Maybe (List Goal))
    | AiPersonMemoriesResponseReceived PersonId (Maybe (List Memory))
    | SidebarMsg Sidebar.Msg
    | DevelopmentDataResponseReceived (Maybe ())


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
    ( Loading (Shared.init key) (DevelopmentDataTarget route)
    , DevelopmentData.init DevelopmentDataResponseReceived
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

        HumanPerson model ->
            HumanPersonPage.shared model

        AiPerson model ->
            AiPersonPage.shared model

        Loading sharedModel _ ->
            sharedModel

        LoadFailed sharedModel ->
            sharedModel

        PageNotFound sharedModel ->
            sharedModel


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

        HumanPerson model ->
            HumanPerson (HumanPersonPage.setShared sharedModel model)

        AiPerson model ->
            AiPerson (AiPersonPage.setShared sharedModel model)

        Loading _ target ->
            Loading sharedModel target

        LoadFailed _ ->
            LoadFailed sharedModel

        PageNotFound _ ->
            PageNotFound sharedModel


handleRouteChange : Maybe Route -> Shared.Model -> ( Page, Eff Msg )
handleRouteChange maybeRoute sharedModel =
    case maybeRoute of
        Just Route.Conversations ->
            ( Loading sharedModel ConversationsTarget
            , E.attempt
                (AllConversationsResponseReceived << AllConversations.flagsFromResponse)
                Conversation.getAllConversationsPageFlags
            )

        Just (Route.Conversation id) ->
            ( Loading sharedModel (ConversationTarget id)
            , E.attempt
                (ConversationResponseReceived id << ConversationPage.flagsFromResponse)
                (Chat.getConversationPageFlags id)
            )

        Just Route.AllPersons ->
            AllPersons.init sharedModel
                |> Tuple.mapFirst AllPersons
                |> Tuple.mapSecond (E.map AllPersonsMsg)

        Just Route.NewPerson ->
            NewPerson.init sharedModel
                |> NewPerson
                |> E.withOut

        Just (Route.Person personId) ->
            ( Loading sharedModel (PersonTarget personId)
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
            case page of
                Loading sharedModel (DevelopmentDataTarget _) ->
                    ( Loading sharedModel (DevelopmentDataTarget maybeRoute)
                    , E.none
                    )

                _ ->
                    handleRouteChange maybeRoute (getShared page)

        DevelopmentDataResponseReceived result ->
            case page of
                Loading sharedModel (DevelopmentDataTarget maybeRoute) ->
                    case result of
                        Just () ->
                            handleRouteChange maybeRoute sharedModel

                        Nothing ->
                            ( LoadFailed sharedModel, E.none )

                _ ->
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
                    if model.conversation.id == id then
                        ConversationPage.update conversationMsg model
                            |> Tuple.mapFirst Conversation
                            |> Tuple.mapSecond (E.map (ConversationMsg id))

                    else
                        ( page, E.none )

                _ ->
                    ( page, E.none )

        HumanPersonMsg humanPersonMsg ->
            case page of
                HumanPerson model ->
                    HumanPersonPage.update humanPersonMsg model
                        |> Tuple.mapFirst HumanPerson
                        |> Tuple.mapSecond (E.map HumanPersonMsg)

                _ ->
                    ( page, E.none )

        AiPersonMsg aiPersonMsg ->
            case page of
                AiPerson model ->
                    AiPersonPage.update aiPersonMsg model
                        |> Tuple.mapFirst AiPerson
                        |> Tuple.mapSecond (E.map AiPersonMsg)

                _ ->
                    ( page, E.none )

        AllConversationsResponseReceived result ->
            case page of
                Loading sharedModel ConversationsTarget ->
                    case result of
                        Just flags ->
                            ( AllConversations (AllConversations.init sharedModel flags)
                            , E.none
                            )

                        Nothing ->
                            ( LoadFailed sharedModel, E.none )

                _ ->
                    ( page, E.none )

        ConversationResponseReceived conversationId result ->
            case page of
                Loading sharedModel (ConversationTarget requestedId) ->
                    if conversationId /= requestedId then
                        ( page, E.none )

                    else
                        case result of
                            Remote.Found flags ->
                                ( Conversation (ConversationPage.init sharedModel flags)
                                , E.none
                                )

                            Remote.Failed ->
                                ( LoadFailed sharedModel, E.none )

                            Remote.NotFound ->
                                ( PageNotFound sharedModel, E.none )

                _ ->
                    ( page, E.none )

        LoadedPersonPage personId maybePersonPageFlags ->
            case page of
                Loading sharedModel (PersonTarget requestedId) ->
                    if personId /= requestedId then
                        ( page, E.none )

                    else
                        case maybePersonPageFlags of
                            Remote.Found flags ->
                                case flags.person.kind of
                                    Person.HumanPerson ->
                                        ( HumanPerson (HumanPersonPage.init sharedModel flags.person)
                                        , E.none
                                        )

                                    Person.AiPerson profile ->
                                        ( Loading sharedModel (AiPersonGoalsTarget flags.person profile)
                                        , E.attempt (AiPersonGoalsResponseReceived personId)
                                            (Goal.getGoals personId)
                                        )

                            Remote.Failed ->
                                ( LoadFailed sharedModel, E.none )

                            Remote.NotFound ->
                                ( PageNotFound sharedModel, E.none )

                _ ->
                    ( page, E.none )

        AiPersonGoalsResponseReceived personId response ->
            case page of
                Loading sharedModel (AiPersonGoalsTarget person profile) ->
                    if personId /= person.id then
                        ( page, E.none )

                    else
                        case response of
                            Just goals ->
                                ( Loading sharedModel (AiPersonMemoriesTarget person profile goals)
                                , E.attempt (AiPersonMemoriesResponseReceived personId)
                                    (Memory.getMemories personId)
                                )

                            Nothing ->
                                ( LoadFailed sharedModel, E.none )

                _ ->
                    ( page, E.none )

        AiPersonMemoriesResponseReceived personId response ->
            case page of
                Loading sharedModel (AiPersonMemoriesTarget person profile goals) ->
                    if personId /= person.id then
                        ( page, E.none )

                    else
                        case response of
                            Just memories ->
                                ( AiPerson
                                    (AiPersonPage.init sharedModel
                                        { person = person
                                        , profile = profile
                                        , goals = goals
                                        , memories = memories
                                        }
                                    )
                                , E.none
                                )

                            Nothing ->
                                ( LoadFailed sharedModel, E.none )

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
            ConversationPage.view model |> Document.map (ConversationMsg model.conversation.id)

        AllPersons allPersonsModel ->
            AllPersons.view allPersonsModel
                |> Document.map AllPersonsMsg

        NewPerson newPersonModel ->
            NewPerson.document newPersonModel
                |> Document.map NewPersonMsg

        HumanPerson model ->
            HumanPersonPage.document model
                |> Document.map HumanPersonMsg

        AiPerson model ->
            AiPersonPage.document model
                |> Document.map AiPersonMsg

        Loading _ _ ->
            { title = "Loading"
            , body =
                [ H.p
                    [ A.css [ S.p4 ], A.attribute "role" "status" ]
                    [ H.text "Loading…" ]
                ]
            }

        LoadFailed _ ->
            { title = "Could not load page"
            , body =
                [ H.p
                    [ A.css [ S.p4 ], A.attribute "role" "alert" ]
                    [ H.text "Could not load page." ]
                ]
            }

        PageNotFound _ ->
            { title = "Page not found"
            , body =
                [ H.main_ []
                    [ H.h1 [] [ H.text "Page not found" ]
                    ]
                ]
            }


subscriptions : Page -> Sub Msg
subscriptions page =
    case page of
        Conversation model ->
            ConversationPage.subscriptions
                |> Sub.map (ConversationMsg model.conversation.id)

        _ ->
            Sub.none
