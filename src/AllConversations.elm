module AllConversations exposing (Flags, Model, Msg, flagsFromResponse, init, setShared, shared, update, view)

import Conversation exposing (Conversation)
import ConversationId exposing (ConversationId)
import ConversationId.Util as ConversationIdUtil
import ConversationTitle
import ConversationTitle.Util as ConversationTitleUtil
import Document exposing (Document)
import Effect as E exposing (Eff)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as Ev
import Person exposing (Person)
import PersonId.Util as PersonIdUtil
import Route
import Shared
import Style as S
import View.Button as Button



----------------------------------------------------------------
-- TYPES
----------------------------------------------------------------


type alias Model =
    { shared : Shared.Model
    , conversations : List Conversation
    , people : List Person
    , title : String
    , speaker : String
    , pending : Bool
    , error : Maybe String
    }


type Msg
    = TitleInputChanged String
    | SpeakerSelectionChanged String
    | CreateButtonClicked
    | ConversationCreatedResponseReceived (Maybe ConversationId)



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
    , speaker = ""
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

        SpeakerSelectionChanged value ->
            ( { model | speaker = value }, E.none )

        CreateButtonClicked ->
            case PersonIdUtil.fromString model.speaker of
                Just personId ->
                    if model.pending || String.isEmpty (String.trim model.title) then
                        ( model, E.none )

                    else
                        ( { model | pending = True, error = Nothing }
                        , E.attempt
                            ConversationCreatedResponseReceived
                            (Conversation.createConversation
                                (ConversationTitle.ConversationTitle (String.trim model.title))
                                personId
                            )
                        )

                Nothing ->
                    ( { model | error = Just "Choose the first participant." }, E.none )

        ConversationCreatedResponseReceived result ->
            case result of
                Just id ->
                    ( { model | pending = False }
                    , E.pushUrl
                        ("/conversation/"
                            ++ ConversationIdUtil.toString id
                        )
                    )

                Nothing ->
                    ( { model
                        | pending = False
                        , error =
                            Just "Could not create the conversation. Your inputs are still here."
                      }
                    , E.none
                    )



----------------------------------------------------------------
-- VIEW
----------------------------------------------------------------


view : Model -> Document Msg
view model =
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
                , case model.error of
                    Just message ->
                        H.p [ A.attribute "role" "status" ] [ H.text message ]

                    Nothing ->
                        H.text ""
                ]
            ]
        ]
    }


conversationList : Model -> Html Msg
conversationList model =
    let
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
            (case model.conversations of
                [] ->
                    [ H.li [] [ H.text "No conversations yet. Create one below." ] ]

                conversations ->
                    List.map conversationLink conversations
            )
        , H.fieldset [ A.attribute "aria-labelledby" "new-conversation-heading", A.disabled model.pending, A.css [ S.border0, S.col, S.g2 ] ]
            [ H.div [ A.css [ S.col, S.g3 ] ]
                [ H.h2 [ A.id "new-conversation-heading", A.css [ S.textGray3 ] ] [ H.text "New conversation" ]
                , H.label
                    [ A.css [ S.col, S.g2 ] ]
                    [ H.text "Title"
                    , H.input
                        [ A.value model.title
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
                , Button.primary "Create conversation" CreateButtonClicked |> Button.toHtml
                ]
            ]
        ]


personSelector : Model -> Html Msg
personSelector model =
    let
        personOption : Person -> Html Msg
        personOption p =
            H.option
                [ A.value (PersonIdUtil.toString p.id)
                , A.selected (model.speaker == PersonIdUtil.toString p.id)
                ]
                [ H.text p.name ]
    in
    H.label [ A.css [ S.col, S.g2 ] ]
        [ H.text "Person"
        , H.select
            [ Ev.onInput SpeakerSelectionChanged
            , A.css
                [ S.selectControl ]
            ]
            (H.option
                [ A.value ""
                , A.selected (model.speaker == "")
                ]
                [ H.text "Choose a person" ]
                :: List.map personOption model.people
            )
        ]
