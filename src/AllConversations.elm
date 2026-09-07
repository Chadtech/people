module AllConversations exposing (Model, Msg, init, setShared, shared, update, view)

import Chat
import ConversationId exposing (ConversationId)
import ConversationId.Util as ConversationIdUtil
import ConversationTitle
import ConversationTitle.Util as ConversationTitleUtil
import Css
import Document exposing (Document)
import Effect as E exposing (Eff)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as Ev
import Person
import PersonId.Util as PersonIdUtil
import Route
import Shared
import Style as S
import View.Button as Button


type alias Model =
    { shared : Shared.Model
    , conversations : Maybe (List Chat.Conversation)
    , people : List Person.Person
    , title : String
    , speaker : String
    , pending : Bool
    , error : Maybe String
    }


type Msg
    = PageFlagsResponseReceived (Maybe (List Chat.AllConversationsPageFlag))
    | TitleInputChanged String
    | SpeakerSelectionChanged String
    | CreateButtonClicked
    | ConversationCreatedResponseReceived (Maybe ConversationId)
    | RetryButtonClicked


init : Shared.Model -> ( Model, Eff Msg )
init sharedModel =
    ( { shared = sharedModel
      , conversations = Nothing
      , people = []
      , title = ""
      , speaker = ""
      , pending = False
      , error = Nothing
      }
    , load
    )


load : Eff Msg
load =
    E.attempt PageFlagsResponseReceived Chat.getAllConversationsPageFlags


shared : Model -> Shared.Model
shared model =
    model.shared


setShared : Shared.Model -> Model -> Model
setShared value model =
    { model | shared = value }


update : Msg -> Model -> ( Model, Eff Msg )
update msg model =
    case msg of
        PageFlagsResponseReceived result ->
            case result of
                Nothing ->
                    ( { model | error = Just "Could not load conversations and people. Check the connection and retry." }
                    , E.none
                    )

                Just rows ->
                    let
                        flags : PageFlags
                        flags =
                            List.foldr addPageFlag
                                { conversations = [], people = [] }
                                rows
                    in
                    ( { model
                        | conversations = Just flags.conversations
                        , people = flags.people
                        , error = Nothing
                      }
                    , E.none
                    )

        TitleInputChanged value ->
            ( { model | title = value }, E.none )

        SpeakerSelectionChanged value ->
            ( { model | speaker = value }, E.none )

        RetryButtonClicked ->
            ( { model | error = Nothing }, load )

        CreateButtonClicked ->
            case PersonIdUtil.fromString model.speaker of
                Just personId ->
                    if model.pending || String.isEmpty (String.trim model.title) then
                        ( model, E.none )

                    else
                        ( { model | pending = True, error = Nothing }
                        , E.attempt
                            ConversationCreatedResponseReceived
                            (Chat.createConversation
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


type alias PageFlags =
    { conversations : List Chat.Conversation
    , people : List Person.Person
    }


addPageFlag : Chat.AllConversationsPageFlag -> PageFlags -> PageFlags
addPageFlag flag flags =
    case flag of
        Chat.ConversationFlag conversation ->
            { flags | conversations = conversation :: flags.conversations }

        Chat.PersonFlag person ->
            { flags | people = person :: flags.people }

        Chat.ConversationAndPersonFlag conversation person ->
            { flags
                | conversations = conversation :: flags.conversations
                , people = person :: flags.people
            }


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
        conversationLink : Chat.Conversation -> Html Msg
        conversationLink c =
            H.li
                []
                [ H.a
                    [ Route.href (Route.Conversation c.id)
                    , A.css [ S.link, Css.property "overflow-wrap" "anywhere" ]
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
                , Css.property "list-style" "none"
                ]
            ]
            (case model.conversations of
                Nothing ->
                    [ H.li []
                        [ H.text
                            (if model.error == Nothing then
                                "Loading conversations…"

                             else
                                "Conversations could not be loaded."
                            )
                        ]
                    ]

                Just [] ->
                    [ H.li [] [ H.text "No conversations yet. Create one below." ] ]

                Just conversations ->
                    List.map conversationLink conversations
            )
        , if model.error /= Nothing then
            Button.secondary "Retry" RetryButtonClicked |> Button.toHtml

          else
            H.text ""
        , H.fieldset [ A.attribute "aria-labelledby" "new-conversation-heading", A.disabled model.pending, A.css [ Css.border (Css.px 0), S.col, S.g2 ] ]
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
        personOption : Person.Person -> Html Msg
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
