module ConversationPage exposing
    ( Model
    , Msg
    , init
    , setShared
    , shared
    , subscriptions
    , update
    , view
    )

import Acadia.Transaction
import Acadia.UInt64 as UInt64
import Acadia.UInt8
import Chat
import ConversationId exposing (ConversationId)
import ConversationTitle.Util as ConversationTitleUtil
import Css
import Dict exposing (Dict)
import Document exposing (Document)
import Effect as E exposing (Eff)
import GenerationError.Util as GenerationErrorUtil
import GenerationId exposing (GenerationId)
import GenerationId.Util as GenerationIdUtil
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as Ev
import IntervalSeconds
import IntervalSeconds.Util as IntervalSecondsUtil
import MessageContent
import MessageContent.Util as MessageContentUtil
import MessageId.Util as MessageIdUtil
import Note.Util as NoteUtil
import Person
import PersonId exposing (PersonId)
import PersonId.Util as PersonIdUtil
import PromptInspection
import PromptSnapshot
import PromptSnapshot.Util as PromptSnapshotUtil
import Remote exposing (Remote)
import Route
import Set exposing (Set)
import Shared
import Style as S
import Time
import TurnCount
import View.Button as Button
import View.Textarea as Textarea


type alias Model =
    { shared : Shared.Model
    , conversations : Maybe (List Chat.Conversation)
    , people : List Person.Person
    , conversationId : ConversationId
    , messages : List Chat.Message
    , participants : List PersonId
    , noteHistory : Maybe (List Chat.NoteRevision)
    , prompts : Dict String String
    , loadingPrompts : Set String
    , loadingNotes : Bool
    , generations : List Chat.GenerationSummary
    , draft : String
    , speaker : String
    , pending : Bool
    , error : Maybe String
    }


type Msg
    = TickReceived Time.Posix
    | ConversationsResponseReceived (Maybe (List Chat.Conversation))
    | PeopleResponseReceived (Maybe (List Person.Person))
    | MessagesResponseReceived ConversationId (Maybe (List Chat.Message))
    | ParticipantsResponseReceived ConversationId (Maybe (List PersonId))
    | GenerationsResponseReceived ConversationId (Maybe (List Chat.GenerationSummary))
    | PromptInspectionClicked GenerationId
    | PromptResponseReceived ConversationId GenerationId (Remote PromptSnapshot.PromptSnapshot)
    | NoteHistoryClicked
    | NoteHistoryResponseReceived ConversationId (Maybe (List Chat.NoteRevision))
    | DraftInputChanged String
    | SpeakerSelectionChanged String
    | SendButtonClicked
    | ReplyButtonClicked
    | RunButtonClicked
    | AutonomyButtonClicked
    | StopButtonClicked
    | AddParticipantButtonClicked
    | MutationResponseReceived (Maybe ())
    | TurnResponseReceived (Maybe GenerationId)


init : Shared.Model -> ConversationId -> ( Model, Eff Msg )
init sharedModel conversationId =
    let
        model : Model
        model =
            { shared = sharedModel
            , conversations = Nothing
            , people = []
            , conversationId = conversationId
            , messages = []
            , participants = []
            , generations = []
            , noteHistory = Nothing
            , loadingNotes = False
            , prompts = Dict.empty
            , loadingPrompts = Set.empty
            , draft = ""
            , speaker = ""
            , pending = False
            , error = Nothing
            }
    in
    ( model, refresh model )


shared : Model -> Shared.Model
shared model =
    model.shared


setShared : Shared.Model -> Model -> Model
setShared value model =
    { model | shared = value }


subscriptions : Sub Msg
subscriptions =
    Time.every 1500 TickReceived


refresh : Model -> Eff Msg
refresh model =
    E.batch
        [ E.attempt ConversationsResponseReceived Chat.getConversations
        , E.attempt PeopleResponseReceived Person.getAllPersons
        , E.attempt (MessagesResponseReceived model.conversationId) (Chat.getMessages model.conversationId)
        , E.attempt (ParticipantsResponseReceived model.conversationId) (Chat.getParticipants model.conversationId)
        , E.attempt (GenerationsResponseReceived model.conversationId) (Chat.getGenerationSummaries model.conversationId)
        ]


current : Model -> Maybe Chat.Conversation
current model =
    model.conversations
        |> Maybe.andThen (List.filter (\c -> c.id == model.conversationId) >> List.head)


update : Msg -> Model -> ( Model, Eff Msg )
update msg model =
    case msg of
        TickReceived _ ->
            if model.pending then
                ( model, E.none )

            else
                ( model, refresh model )

        ConversationsResponseReceived result ->
            case result of
                Nothing ->
                    ( { model | error = Just "Could not refresh conversations. Check the connection." }
                    , E.none
                    )

                Just rows ->
                    ( { model | conversations = Just rows }, E.none )

        PeopleResponseReceived result ->
            case result of
                Nothing ->
                    ( { model | error = Just "Could not load people." }, E.none )

                Just rows ->
                    ( { model | people = rows }, E.none )

        MessagesResponseReceived id result ->
            if id == model.conversationId then
                ( { model | messages = Maybe.withDefault model.messages result }
                , E.none
                )

            else
                ( model, E.none )

        ParticipantsResponseReceived id result ->
            if id == model.conversationId then
                ( { model | participants = Maybe.withDefault model.participants result }
                , E.none
                )

            else
                ( model, E.none )

        GenerationsResponseReceived id result ->
            if id == model.conversationId then
                ( { model | generations = Maybe.withDefault model.generations result }
                , E.none
                )

            else
                ( model, E.none )

        PromptInspectionClicked generationId ->
            if Set.member (GenerationIdUtil.toString generationId) model.loadingPrompts then
                ( model, E.none )

            else
                ( { model
                    | loadingPrompts =
                        Set.insert
                            (GenerationIdUtil.toString generationId)
                            model.loadingPrompts
                  }
                , E.fetch
                    (PromptResponseReceived model.conversationId generationId)
                    (Chat.getGenerationPrompt model.conversationId generationId)
                )

        PromptResponseReceived conversationId generationId result ->
            if model.conversationId /= conversationId then
                ( model, E.none )

            else
                let
                    next : Model
                    next =
                        { model
                            | loadingPrompts =
                                Set.remove
                                    (GenerationIdUtil.toString generationId)
                                    model.loadingPrompts
                        }
                in
                case result of
                    Remote.Found prompt ->
                        ( { next
                            | prompts =
                                Dict.insert
                                    (GenerationIdUtil.toString
                                        generationId
                                    )
                                    (PromptSnapshotUtil.toString prompt)
                                    next.prompts
                          }
                        , E.none
                        )

                    Remote.NotFound ->
                        ( { next | error = Just "That prompt was not found." }, E.none )

                    Remote.Failed ->
                        ( { next | error = Just "Could not load that prompt. Try again." }, E.none )

        NoteHistoryClicked ->
            if model.loadingNotes then
                ( model, E.none )

            else
                ( { model | loadingNotes = True }
                , E.attempt (NoteHistoryResponseReceived model.conversationId) (Chat.getNoteRevisions model.conversationId)
                )

        NoteHistoryResponseReceived id result ->
            if id == model.conversationId then
                ( { model
                    | loadingNotes = False
                    , noteHistory =
                        case result of
                            Just revisions ->
                                Just revisions

                            Nothing ->
                                model.noteHistory
                    , error =
                        if result == Nothing then
                            Just "Could not load note history. Try again."

                        else
                            model.error
                  }
                , E.none
                )

            else
                ( model, E.none )

        DraftInputChanged value ->
            ( { model | draft = value }, E.none )

        SpeakerSelectionChanged value ->
            ( { model | speaker = value }, E.none )

        SendButtonClicked ->
            if String.isEmpty (String.trim model.draft) then
                ( model, E.none )

            else
                requestTurn model.draft model

        ReplyButtonClicked ->
            requestTurn "" model

        RunButtonClicked ->
            mutateConversation
                (\c -> Chat.setRunLength c.id c.revision (TurnCount.TurnCount (Acadia.UInt8.fromInt 6)))
                model

        AutonomyButtonClicked ->
            mutateConversation
                (\c ->
                    Chat.setAutonomy c.id
                        c.revision
                        True
                        (IntervalSeconds.IntervalSeconds
                            (UInt64.fromInt 300)
                        )
                )
                model

        StopButtonClicked ->
            mutateConversation
                (\c -> Chat.stopConversation c.id)
                model

        AddParticipantButtonClicked ->
            case PersonIdUtil.fromString model.speaker of
                Nothing ->
                    ( { model | error = Just "Choose a person to add." }, E.none )

                Just personId ->
                    mutateConversation (\c -> Chat.addParticipant c.id personId) model

        MutationResponseReceived result ->
            let
                next : Model
                next =
                    { model
                        | pending = False
                        , error =
                            if result == Nothing then
                                Just "Could not apply the change. Refresh and try again."

                            else
                                Nothing
                    }
            in
            ( next, refresh next )

        TurnResponseReceived result ->
            let
                next : Model
                next =
                    { model
                        | pending = False
                        , draft =
                            if result == Nothing then
                                model.draft

                            else
                                ""
                        , error =
                            if result == Nothing then
                                Just
                                    ("Could not start the turn. The conversation may have changed; "
                                        ++ "your draft is still here."
                                    )

                            else
                                Nothing
                    }
            in
            ( next, refresh next )


mutateConversation :
    (Chat.Conversation -> Acadia.Transaction.Transaction ())
    -> Model
    -> ( Model, Eff Msg )
mutateConversation transaction model =
    case current model of
        Just conversation ->
            if model.pending then
                ( model, E.none )

            else
                ( { model | pending = True }
                , E.attempt MutationResponseReceived (transaction conversation)
                )

        Nothing ->
            ( model, E.none )


requestTurn : String -> Model -> ( Model, Eff Msg )
requestTurn content model =
    case ( current model, PersonIdUtil.fromString model.speaker ) of
        ( Just conversation, Just speaker ) ->
            if model.pending || conversation.activeGeneration /= Nothing then
                ( model, E.none )

            else if not (List.member speaker model.participants) then
                ( { model | error = Just "Add this person to the conversation first." }, E.none )

            else
                ( { model | pending = True, error = Nothing }
                , E.attempt TurnResponseReceived
                    (Chat.requestTurn conversation.id
                        conversation.revision
                        speaker
                        (MessageContent.MessageContent content)
                    )
                )

        _ ->
            ( { model | error = Just "Choose who should reply." }, E.none )


view : Model -> Document Msg
view model =
    { title = current model |> Maybe.map (.title >> ConversationTitleUtil.toString) |> Maybe.withDefault "Conversation"
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
                [ H.h1 [ A.css [ S.textGray3 ] ] [ H.text "Conversation" ]
                , case current model of
                    Nothing ->
                        H.p []
                            [ H.text
                                (if model.conversations == Nothing then
                                    "Loading…"

                                 else
                                    "Conversation not found."
                                )
                            ]

                    Just conversation ->
                        conversationView model conversation
                , H.p [ A.attribute "role" "status" ] [ H.text (Maybe.withDefault "" model.error) ]
                ]
            ]
        ]
    }


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


conversationView : Model -> Chat.Conversation -> Html Msg
conversationView model conversation =
    H.div [ A.css [ S.col, S.g3 ] ]
        [ H.a
            [ Route.href Route.Conversations
            , A.css
                [ S.link
                ]
            ]
            [ H.text "All conversations" ]
        , H.h2 [ A.css [ S.textGray3 ] ] [ H.text (ConversationTitleUtil.toString conversation.title) ]
        , H.p []
            [ H.text
                ("Participants: "
                    ++ String.join ", "
                        (List.map
                            (personName model)
                            model.participants
                        )
                )
            ]
        , H.p [ A.attribute "role" "status" ]
            [ H.text
                (if conversation.autonomous then
                    "Autonomy is on: one turn every "
                        ++ IntervalSecondsUtil.toString
                            conversation.intervalSeconds
                        ++ " seconds while the worker runs. OpenAI calls are paid. Stop turns autonomy off."

                 else
                    "Autonomy is off. You can run individual replies, a short "
                        ++ "conversation, or enable ongoing turns."
                )
            ]
        , H.fieldset [ A.disabled model.pending, A.css [ Css.border (Css.px 0), S.col, S.g2 ] ]
            [ personSelector model
            , Button.secondary "Add participant" AddParticipantButtonClicked |> Button.toHtml
            ]
        , H.div [ A.css [ S.col, S.g3 ] ]
            (List.sortBy (\m -> String.padLeft 20 '0' (MessageIdUtil.toString m.id))
                model.messages
                |> List.map (messageView model)
            )
        , H.fieldset [ A.disabled model.pending, A.css [ Css.border (Css.px 0), S.col, S.g2 ] ]
            [ H.label [ A.css [ S.col, S.g2 ] ]
                [ H.text "Message"
                , Textarea.simple
                    model.draft
                    DraftInputChanged
                    |> Textarea.toHtml
                ]
            , if conversation.activeGeneration == Nothing then
                H.div [ A.css [ S.row, S.g2, S.flexWrap ] ]
                    [ Button.primary "Send" SendButtonClicked |> Button.toHtml
                    , Button.secondary "Let selected person reply"
                        ReplyButtonClicked
                        |> Button.toHtml
                    , Button.secondary "Run 6 turns" RunButtonClicked |> Button.toHtml
                    , if conversation.autonomous then
                        H.text ""

                      else
                        Button.secondary "Enable autonomy every 5 minutes"
                            AutonomyButtonClicked
                            |> Button.toHtml
                    ]

              else
                H.p [ A.attribute "role" "status" ]
                    [ H.text
                        "Waiting for a reply… The Haskell worker must be running."
                    ]
            , Button.secondary "Stop" StopButtonClicked |> Button.toHtml
            ]
        , if String.isEmpty (NoteUtil.toString conversation.note) then
            H.text ""

          else
            H.section [ A.css [ S.col, S.g2 ] ]
                [ H.h2 [ A.css [ S.textGray3 ] ]
                    [ H.text "Shared note"
                    ]
                , pre (NoteUtil.toString conversation.note)
                ]
        , H.section [ A.css [ S.col, S.g2 ] ]
            [ H.h2 [ A.css [ S.textGray3 ] ] [ H.text "Note history" ]
            , H.fieldset [ A.disabled model.loadingNotes, A.css [ Css.border (Css.px 0), S.minW0 ] ]
                [ Button.secondary
                    (if model.loadingNotes then
                        "Loading note history…"

                     else
                        "Load / refresh note history"
                    )
                    NoteHistoryClicked
                    |> Button.toHtml
                ]
            , case model.noteHistory of
                Nothing ->
                    H.text ""

                Just [] ->
                    H.p [] [ H.text "No note revisions yet." ]

                Just revisions ->
                    H.div [ A.css [ S.col, S.g2 ] ]
                        (revisions
                            |> List.sortBy
                                (\revision ->
                                    String.padLeft 20
                                        '0'
                                        (GenerationIdUtil.toString revision.generationId)
                                )
                            |> List.reverse
                            |> List.map
                                (\revision ->
                                    H.details [ A.css [ S.col, S.g2 ] ]
                                        [ H.summary []
                                            [ H.text
                                                ("Turn "
                                                    ++ GenerationIdUtil.toString
                                                        revision.generationId
                                                    ++ " — "
                                                    ++ personName model revision.author
                                                )
                                            ]
                                        , pre (NoteUtil.toString revision.content)
                                        ]
                                )
                        )
            ]
        , H.section [ A.css [ S.col, S.g2 ] ]
            [ H.h2 [ A.css [ S.textGray3 ] ] [ H.text "Generation history" ]
            , H.div []
                (List.sortBy
                    (\g ->
                        String.padLeft 20
                            '0'
                            (GenerationIdUtil.toString g.id)
                    )
                    model.generations
                    |> List.reverse
                    |> List.map (generationView model)
                )
            ]
        ]


personName : Model -> PersonId -> String
personName model id =
    model.people
        |> List.filter (\p -> p.id == id)
        |> List.head
        |> Maybe.map .name
        |> Maybe.withDefault "Unknown person"


messageView : Model -> Chat.Message -> Html msg
messageView model message =
    H.section [ A.css [ S.col, S.g2 ] ]
        [ H.h3 [ A.css [ S.textGray3 ] ]
            [ H.text
                (Maybe.map (personName model)
                    message.author
                    |> Maybe.withDefault "You"
                )
            ]
        , pre (MessageContentUtil.toString message.content)
        ]


pre : String -> Html msg
pre content =
    H.pre
        [ A.css
            [ Css.whiteSpace Css.preWrap
            , Css.property "overflow-wrap" "anywhere"
            , S.minW0
            ]
        ]
        [ H.text content ]


generationView : Model -> Chat.GenerationSummary -> Html Msg
generationView model generation =
    H.details [ A.css [ S.col, S.g2 ] ]
        [ H.summary []
            [ H.text
                ("Turn "
                    ++ GenerationIdUtil.toString generation.id
                    ++ " — "
                    ++ phaseName generation.phase
                )
            ]
        , H.p [] [ H.text (GenerationErrorUtil.toString generation.error) ]
        , H.fieldset
            [ A.disabled
                (Set.member
                    (GenerationIdUtil.toString
                        generation.id
                    )
                    model.loadingPrompts
                )
            , A.css [ Css.border (Css.px 0), S.minW0 ]
            ]
            [ Button.secondary "Load / refresh prompt"
                (PromptInspectionClicked
                    generation.id
                )
                |> Button.toHtml
            ]
        , case Dict.get (GenerationIdUtil.toString generation.id) model.prompts of
            Nothing ->
                H.text ""

            Just "" ->
                H.p [] [ H.text "No prompt has been saved for this turn yet." ]

            Just prompt ->
                PromptInspection.view prompt
        ]


phaseName : Chat.Phase -> String
phaseName phase =
    case phase of
        Chat.Pending ->
            "Queued"

        Chat.Running ->
            "Generating"

        Chat.Completed ->
            "Complete"

        Chat.Failed ->
            "Failed"

        Chat.Cancelled ->
            "Stopped"
