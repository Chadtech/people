module AllConversations exposing (Flags, Model, Msg, flagsFromResponse, init, setShared, shared, update, view)

import Conversation exposing (Conversation)
import ConversationId exposing (ConversationId)
import ConversationTitle
import ConversationTitle.Util as ConversationTitleUtil
import Css
import Document exposing (Document)
import Effect as E exposing (Eff)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as Ev
import Json.Decode as Decode
import Person exposing (Person)
import PersonId exposing (PersonId)
import Route
import Shared
import Style as S
import View.Button as Button
import View.TextField as TextField



----------------------------------------------------------------
-- TYPES
----------------------------------------------------------------


type alias Model =
    { shared : Shared.Model
    , conversations : List Conversation
    , people : List Person
    , title : String
    , participantSearch : String
    , selectedParticipants : List PersonId
    , createdConversation : Maybe ConversationId
    , remainingParticipants : List PersonId
    , pending : Bool
    , error : Maybe String
    }


type Msg
    = TitleInputChanged String
    | ParticipantSearchInputChanged String
    | PersonResultClicked PersonId
    | RemoveParticipantButtonClicked PersonId
    | CreateButtonClicked
    | ConversationCreatedResponseReceived (Maybe ConversationId)
    | ParticipantAddedResponseReceived (Maybe ())



----------------------------------------------------------------
-- INIT
----------------------------------------------------------------


type alias Flags =
    { conversations : List Conversation
    , people : List Person
    }


init : Shared.Model -> Flags -> Model
init sharedModel flags =
    { shared = sharedModel
    , conversations = flags.conversations
    , people = flags.people
    , title = ""
    , participantSearch = ""
    , selectedParticipants = []
    , createdConversation = Nothing
    , remainingParticipants = []
    , pending = False
    , error = Nothing
    }


flagsFromResponse :
    Maybe (List Conversation.AllConversationsPageFlag)
    -> Maybe Flags
flagsFromResponse =
    let
        addPageFlag : Conversation.AllConversationsPageFlag -> Flags -> Flags
        addPageFlag flag flags =
            case flag of
                Conversation.ConversationFlag conversation ->
                    { flags | conversations = conversation :: flags.conversations }

                Conversation.PersonFlag person ->
                    { flags | people = person :: flags.people }
    in
    Maybe.map
        (List.foldr addPageFlag
            { conversations = [], people = [] }
        )



----------------------------------------------------------------
-- API
----------------------------------------------------------------


shared : Model -> Shared.Model
shared model =
    model.shared


setShared : Shared.Model -> Model -> Model
setShared value model =
    { model | shared = value }



----------------------------------------------------------------
-- UPDATE
----------------------------------------------------------------


update : Msg -> Model -> ( Model, Eff Msg )
update msg model =
    case msg of
        TitleInputChanged value ->
            ( { model | title = value }, E.none )

        ParticipantSearchInputChanged value ->
            ( { model | participantSearch = value }, E.none )

        PersonResultClicked personId ->
            if model.pending || model.createdConversation /= Nothing || List.member personId model.selectedParticipants then
                ( model, E.none )

            else
                ( { model | selectedParticipants = model.selectedParticipants ++ [ personId ] }
                , E.none
                )

        RemoveParticipantButtonClicked personId ->
            if model.pending || model.createdConversation /= Nothing then
                ( model, E.none )

            else
                ( { model | selectedParticipants = List.filter ((/=) personId) model.selectedParticipants }
                , E.none
                )

        CreateButtonClicked ->
            if model.pending then
                ( model, E.none )

            else
                case model.createdConversation of
                    Just id ->
                        addRemainingParticipants id model

                    Nothing ->
                        case model.selectedParticipants of
                            [] ->
                                ( { model | error = Just "Choose at least one participant." }, E.none )

                            first :: remaining ->
                                if String.isEmpty (String.trim model.title) then
                                    ( { model | error = Just "Enter a conversation title." }, E.none )

                                else
                                    ( { model | pending = True, error = Nothing, remainingParticipants = remaining }
                                    , E.attempt ConversationCreatedResponseReceived
                                        (Conversation.createConversation
                                            (ConversationTitle.ConversationTitle (String.trim model.title))
                                            first
                                        )
                                    )

        ConversationCreatedResponseReceived result ->
            case result of
                Just id ->
                    addRemainingParticipants id { model | createdConversation = Just id }

                Nothing ->
                    ( { model | pending = False, error = Just "Could not create the conversation. Your inputs are still here." }
                    , E.none
                    )

        ParticipantAddedResponseReceived result ->
            case ( result, model.createdConversation ) of
                ( Just (), Just id ) ->
                    addRemainingParticipants id
                        { model | remainingParticipants = List.drop 1 model.remainingParticipants }

                _ ->
                    ( { model | pending = False, error = Just "The conversation was created, but some participants could not be added. Retry to finish adding them." }
                    , E.none
                    )


addRemainingParticipants : ConversationId -> Model -> ( Model, Eff Msg )
addRemainingParticipants id model =
    case model.remainingParticipants of
        [] ->
            ( { model | pending = False }, E.pushRoute (Route.Conversation id) )

        personId :: _ ->
            ( { model | pending = True, error = Nothing }
            , E.attempt ParticipantAddedResponseReceived (Conversation.addParticipant id personId)
            )



----------------------------------------------------------------
-- VIEW
----------------------------------------------------------------


view : Model -> Document Msg
view model =
    let
        errorStatus : Html Msg
        errorStatus =
            case model.error of
                Just message ->
                    H.p [ A.attribute "role" "status" ] [ H.text message ]

                Nothing ->
                    H.text ""
    in
    { title = "Conversations"
    , body =
        [ H.div [ A.css [ S.p4, S.col, S.g3, S.wFull ] ]
            [ H.article
                [ A.css
                    [ S.bgGray1
                    , S.outdent
                    , S.p3
                    , S.col
                    , S.g3
                    , S.minW0
                    , S.wFull
                    ]
                ]
                [ H.h1 [ A.css [ S.textGray3 ] ] [ H.text "Conversations" ]
                , conversationList model
                , errorStatus
                ]
            ]
        ]
    }


conversationList : Model -> Html Msg
conversationList model =
    let
        createLabel : String
        createLabel =
            if model.createdConversation /= Nothing then
                "Retry adding participants"

            else
                "Create conversation"

        conversationLink : Conversation -> Html Msg
        conversationLink c =
            H.li
                []
                [ H.a
                    [ Route.href (Route.Conversation c.id)
                    , A.css [ S.link, S.wrapAnywhere ]
                    ]
                    [ H.text (ConversationTitleUtil.toString c.title) ]
                ]

        conversationItems : List (Html Msg)
        conversationItems =
            case model.conversations of
                [] ->
                    [ H.li [] [ H.text "No conversations yet. Create one below." ] ]

                conversations ->
                    List.map conversationLink conversations
    in
    H.div
        [ A.css [ S.col, S.g4 ] ]
        [ H.ul
            [ A.css
                [ S.col
                , S.g2
                , S.listNone
                ]
            ]
            conversationItems
        , H.fieldset [ A.attribute "aria-labelledby" "new-conversation-heading", A.disabled model.pending, A.css [ S.border0, S.col, S.g2 ] ]
            [ H.div [ A.css [ S.col, S.g3 ] ]
                [ H.h2 [ A.id "new-conversation-heading", A.css [ S.textGray3 ] ] [ H.text "New conversation" ]
                , H.label
                    [ A.css [ S.col, S.g2 ] ]
                    [ H.text "Title"
                    , H.input
                        [ A.disabled (model.createdConversation /= Nothing)
                        , A.value model.title
                        , Ev.onInput TitleInputChanged
                        , A.css
                            [ S.indent
                            , S.bgNightwood1
                            , S.textGray4
                            , S.p2
                            , S.wFull
                            ]
                        ]
                        []
                    ]
                , personSelector model
                , Button.primary createLabel CreateButtonClicked |> Button.toHtml
                ]
            ]
        ]


personSelector : Model -> Html Msg
personSelector model =
    let
        availablePeople : List Person
        availablePeople =
            model.people
                |> List.filter
                    (\person ->
                        not (List.member person.id model.selectedParticipants)
                            && String.contains
                                (String.toLower (String.trim model.participantSearch))
                                (String.toLower person.name)
                    )
                |> List.sortBy (.name >> String.toLower)

        selectionDisabled : Bool
        selectionDisabled =
            model.pending || model.createdConversation /= Nothing

        rowTabIndex : Int
        rowTabIndex =
            if selectionDisabled then
                -1

            else
                0

        personResult : Int -> Person -> Html Msg
        personResult index person =
            let
                keyPressed : Decode.Decoder ( Msg, Bool )
                keyPressed =
                    Decode.field "key" Decode.string
                        |> Decode.andThen
                            (\key ->
                                if key == "Enter" || key == " " then
                                    Decode.succeed ( PersonResultClicked person.id, True )

                                else
                                    Decode.fail "Not a selection key"
                            )
            in
            H.li
                [ A.attribute "role" "option"
                , A.attribute "aria-selected" "false"
                , A.tabindex rowTabIndex
                , Ev.onClick (PersonResultClicked person.id)
                , Ev.preventDefaultOn "keydown" keyPressed
                , A.css [ S.selectableListRow index ]
                ]
                [ H.text person.name ]

        searchResults : List (Html Msg)
        searchResults =
            if List.isEmpty availablePeople then
                [ H.li [ A.css [ S.p2 ] ] [ H.text "No people match your search who are not already selected." ] ]

            else
                List.indexedMap personResult availablePeople

        selectedPeople : List Person
        selectedPeople =
            model.selectedParticipants
                |> List.filterMap
                    (\personId ->
                        model.people
                            |> List.filter (\person -> person.id == personId)
                            |> List.head
                    )

        selectedPerson : Person -> Html Msg
        selectedPerson person =
            H.li [ A.css [ S.row, S.g2, S.wrapAnywhere, S.itemsCenter ] ]
                [ H.span [] [ H.text person.name ]
                , H.button
                    [ A.type_ "button"
                    , A.attribute "aria-label" ("Remove " ++ person.name)
                    , A.title ("Remove " ++ person.name)
                    , Ev.onClick (RemoveParticipantButtonClicked person.id)
                    , A.css
                        [ S.border0
                        , S.bgNone
                        , S.textGray4
                        , S.w6
                        , S.h6
                        , S.flex00auto
                        , S.pointerCursor
                        , Css.hover [ S.bgNightwood1, S.textGray5 ]
                        , Css.focus [ S.bgNightwood1, S.textGray5 ]
                        ]
                    ]
                    [ H.text "×" ]
                ]

        participantList : Html Msg
        participantList =
            if List.isEmpty selectedPeople then
                H.p [] [ H.text "No participants selected yet." ]

            else
                H.ul [ A.css [ S.col, S.g2, S.listNone ] ]
                    (List.map selectedPerson selectedPeople)
    in
    H.fieldset
        [ A.disabled (model.createdConversation /= Nothing)
        , A.css [ S.border0, S.minW0 ]
        ]
        [ H.legend [ A.css [ S.textGray3 ] ] [ H.text "Participants" ]
        , H.div [ A.css [ S.col, S.g3 ] ]
            [ H.p [] [ H.text "Select people to add them below. Your own participation is optional." ]
            , H.label [ A.css [ S.col, S.g2 ] ]
                [ H.text "Search people"
                , TextField.simple model.participantSearch ParticipantSearchInputChanged
                    |> TextField.toHtml
                ]
            , H.ul
                [ A.attribute "role" "listbox"
                , A.attribute "aria-label" "People to add"
                , A.css [ S.col, S.listNone, S.maxH64, S.overflowAuto, S.bgNightwood0, S.indent ]
                ]
                searchResults
            , H.section [ A.css [ S.col, S.g2 ] ]
                [ H.h3 [ A.css [ S.textGray3 ] ] [ H.text "People joining this conversation" ]
                , participantList
                ]
            ]
        ]
