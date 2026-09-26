module ConversationPage exposing
    ( Flags
    , Model
    , Msg
    , flagsFromResponse
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
import Chat exposing (ConversationPageFlag, Message)
import Conversation exposing (Conversation)
import ConversationId exposing (ConversationId)
import ConversationTitle.Util as ConversationTitleUtil
import Css
import Dict exposing (Dict)
import Document exposing (Document)
import Effect as E exposing (Eff)
import Generation exposing (GenerationSummary)
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
import Person exposing (Person)
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



----------------------------------------------------------------
-- TYPES
----------------------------------------------------------------


type alias Model =
    { shared : Shared.Model
    , conversation : Conversation
    , people : List Person
    , messages : List Chat.Message
    , participants : List PersonId
    , noteHistory : Maybe (List Chat.NoteRevision)
    , prompts : Dict String String
    , loadingPrompts : Set String
    , loadingNotes : Bool
    , generations : List Generation.GenerationSummary
    , draft : String
    , speaker : String
    , speakerError : Maybe String
    , pending : Bool
    , error : Maybe String
    }


type Msg
    = TickReceived
    | PageFlagsResponseReceived (Remote Flags)
    | PromptInspectionClicked GenerationId
    | PromptResponseReceived GenerationId (Remote PromptSnapshot.PromptSnapshot)
    | NoteHistoryClicked
    | NoteHistoryResponseReceived (Maybe (List Chat.NoteRevision))
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



----------------------------------------------------------------
-- INIT
----------------------------------------------------------------


type alias Flags =
    { conversation : Conversation
    , people : List Person
    , messages : List Message
    , participants : List PersonId
    , generations : List GenerationSummary
    }


type alias PageFlags =
    { conversation : Maybe Conversation
    , people : List Person
    , messages : List Message
    , participants : List PersonId
    , generations : List Generation.GenerationSummary
    }


init : Shared.Model -> Flags -> Model
init sharedModel flags =
    { shared = sharedModel
    , conversation = flags.conversation
    , people = flags.people
    , messages = flags.messages
    , participants = flags.participants
    , generations = flags.generations
    , noteHistory = Nothing
    , loadingNotes = False
    , prompts = Dict.empty
    , loadingPrompts = Set.empty
    , draft = ""
    , speaker = ""
    , speakerError = Nothing
    , pending = False
    , error = Nothing
    }


addPageFlag : Chat.ConversationPageFlag -> PageFlags -> PageFlags
addPageFlag flag flags =
    case flag of
        Chat.ConversationFlag conversation ->
            { flags | conversation = Just conversation }

        Chat.PersonFlag person ->
            { flags | people = person :: flags.people }

        Chat.MessageFlag message ->
            { flags | messages = message :: flags.messages }

        Chat.ParticipantFlag personId ->
            { flags | participants = personId :: flags.participants }

        Chat.GenerationFlag generation ->
            { flags | generations = generation :: flags.generations }



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
-- HELPERS
----------------------------------------------------------------


refresh : ConversationId -> Eff Msg
refresh conversationId =
    E.attempt (PageFlagsResponseReceived << flagsFromResponse)
        (Chat.getConversationPageFlags conversationId)


flagsFromResponse : Maybe (List ConversationPageFlag) -> Remote Flags
flagsFromResponse result =
    case result of
        Nothing ->
            Remote.NotFound

        Just rows ->
            let
                flags : PageFlags
                flags =
                    List.foldr addPageFlag
                        { conversation = Nothing
                        , people = []
                        , messages = []
                        , participants = []
                        , generations = []
                        }
                        rows
            in
            case flags.conversation of
                Nothing ->
                    Remote.NotFound

                Just conversation ->
                    Remote.Found
                        { conversation = conversation
                        , people = flags.people
                        , messages = flags.messages
                        , participants = flags.participants
                        , generations = flags.generations
                        }


mutateConversation :
    (Conversation -> Acadia.Transaction.Transaction ())
    -> Model
    -> ( Model, Eff Msg )
mutateConversation transaction model =
    if model.pending then
        ( model, E.none )

    else
        ( { model | pending = True }
        , E.attempt MutationResponseReceived (transaction model.conversation)
        )


requestTurn : String -> Model -> ( Model, Eff Msg )
requestTurn content model =
    let
        selectedSpeaker : Maybe PersonId
        selectedSpeaker =
            if String.isEmpty model.speaker then
                Nothing

            else
                PersonIdUtil.fromString model.speaker
    in
    case selectedSpeaker of
        Just speaker ->
            if model.pending || model.conversation.activeGeneration /= Nothing then
                ( model, E.none )

            else if not (List.member speaker model.participants) then
                ( { model | speakerError = Just "Add the selected person to the conversation, then try again." }, E.none )

            else
                ( { model | pending = True, error = Nothing, speakerError = Nothing }
                , E.attempt TurnResponseReceived
                    (Chat.requestTurn model.conversation.id
                        model.conversation.revision
                        speaker
                        (MessageContent.MessageContent content)
                    )
                )

        _ ->
            ( { model | speakerError = Just "Choose who should reply from the Person menu above, then try again." }, E.none )



----------------------------------------------------------------
-- UPDATE
----------------------------------------------------------------


update : Msg -> Model -> ( Model, Eff Msg )
update msg model =
    case msg of
        TickReceived ->
            if model.pending then
                ( model, E.none )

            else
                ( model, refresh model.conversation.id )

        PageFlagsResponseReceived result ->
            case result of
                Remote.Failed ->
                    ( { model | error = Just "Could not refresh the conversation. Check the connection." }
                    , E.none
                    )

                Remote.NotFound ->
                    ( { model | error = Just "This conversation is no longer available. Return to All conversations." }
                    , E.none
                    )

                Remote.Found flags ->
                    ( { model
                        | conversation = flags.conversation
                        , people = flags.people
                        , messages = flags.messages
                        , participants = flags.participants
                        , generations = flags.generations
                      }
                    , E.none
                    )

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
                    (PromptResponseReceived generationId)
                    (Generation.getGenerationPrompt model.conversation.id generationId)
                )

        PromptResponseReceived generationId result ->
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
                , E.attempt NoteHistoryResponseReceived (Chat.getNoteRevisions model.conversation.id)
                )

        NoteHistoryResponseReceived result ->
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

        DraftInputChanged value ->
            ( { model | draft = value }, E.none )

        SpeakerSelectionChanged value ->
            ( { model | speaker = value, speakerError = Nothing }
            , E.none
            )

        SendButtonClicked ->
            if String.isEmpty (String.trim model.draft) then
                ( model, E.none )

            else
                requestTurn model.draft model

        ReplyButtonClicked ->
            requestTurn "" model

        RunButtonClicked ->
            mutateConversation
                (\c ->
                    Conversation.setRunLength
                        c.id
                        c.revision
                        (TurnCount.TurnCount (Acadia.UInt8.fromInt 6))
                )
                model

        AutonomyButtonClicked ->
            mutateConversation
                (\c ->
                    Conversation.setAutonomy c.id
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
                    mutateConversation (\c -> Conversation.addParticipant c.id personId) model

        MutationResponseReceived result ->
            ( { model
                | pending = False
                , error =
                    if result == Nothing then
                        Just "Could not apply the change. Refresh and try again."

                    else
                        Nothing
              }
            , refresh model.conversation.id
            )

        TurnResponseReceived result ->
            ( { model
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
            , refresh model.conversation.id
            )



----------------------------------------------------------------
-- VIEW
----------------------------------------------------------------


view : Model -> Document Msg
view model =
    { title = ConversationTitleUtil.toString model.conversation.title
    , body =
        [ H.div
            [ A.css
                [ S.p4, S.col, S.g3, S.wFull ]
            ]
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
                , conversationView model model.conversation
                , H.p [ A.attribute "role" "status" ] [ H.text (Maybe.withDefault "" model.error) ]
                ]
            ]
        ]
    }


conversationView : Model -> Conversation -> Html Msg
conversationView model conversation =
    let
        autonomyStatus : String
        autonomyStatus =
            if conversation.autonomous then
                "Autonomy is on: one turn every "
                    ++ IntervalSecondsUtil.toString conversation.intervalSeconds
                    ++ " seconds while the worker runs. OpenAI calls are paid. Stop turns autonomy off."

            else
                "Autonomy is off. You can run individual replies, a short "
                    ++ "conversation, or enable ongoing turns."

        sharedNote : Html Msg
        sharedNote =
            if String.isEmpty (NoteUtil.toString conversation.note) then
                H.text ""

            else
                H.section [ A.css [ S.col, S.g2 ] ]
                    [ H.h2 [ A.css [ S.textGray3 ] ] [ H.text "Shared note" ]
                    , pre (NoteUtil.toString conversation.note)
                    ]

        messages : Html Msg
        messages =
            let
                messageView : Chat.Message -> Html Msg
                messageView message =
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
            in
            H.div [ A.css [ S.col, S.g3 ] ]
                (model.messages
                    |> List.sortBy (\m -> String.padLeft 20 '0' (MessageIdUtil.toString m.id))
                    |> List.map messageView
                )

        generationHistory : Html Msg
        generationHistory =
            H.section [ A.css [ S.col, S.g2 ] ]
                [ H.h2 [ A.css [ S.textGray3 ] ] [ H.text "Generation history" ]
                , H.div []
                    (model.generations
                        |> List.sortBy (\g -> String.padLeft 20 '0' (GenerationIdUtil.toString g.id))
                        |> List.reverse
                        |> List.map (generationView model)
                    )
                ]

        speakerInvalid : String
        speakerInvalid =
            if model.speakerError /= Nothing then
                "true"

            else
                "false"

        personSelector : Html Msg
        personSelector =
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
                    , A.attribute "aria-describedby" "speaker-feedback"
                    , A.attribute "aria-invalid" speakerInvalid
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
    in
    H.div
        [ A.css
            [ S.col
            , S.g3
            ]
        ]
        [ H.a
            [ Route.href Route.Conversations, A.css [ S.link ] ]
            [ H.text "All conversations" ]
        , H.h2 [ A.css [ S.textGray3 ] ]
            [ H.text (ConversationTitleUtil.toString conversation.title) ]
        , H.p []
            [ H.text
                ("Participants: "
                    ++ String.join ", " (List.map (personName model) model.participants)
                )
            ]
        , H.p [ A.attribute "role" "status" ] [ H.text autonomyStatus ]
        , H.fieldset
            [ A.disabled model.pending
            , A.css [ Css.border (Css.px 0), S.col, S.g2 ]
            ]
            [ personSelector
            , Button.secondary "Add participant" AddParticipantButtonClicked
                |> Button.toHtml
            ]
        , messages
        , composerView model conversation
        , sharedNote
        , noteHistoryView model
        , generationHistory
        ]


composerView : Model -> Conversation -> Html Msg
composerView model conversation =
    let
        speakerFeedback : Html Msg
        speakerFeedback =
            case model.speakerError of
                Nothing ->
                    H.text ""

                Just message ->
                    H.p [ A.css [ S.textRed1 ] ] [ H.text message ]

        replyControls : Html Msg
        replyControls =
            let
                autonomyButton : Html Msg
                autonomyButton =
                    if conversation.autonomous then
                        H.text ""

                    else
                        Button.secondary "Enable autonomy every 5 minutes"
                            AutonomyButtonClicked
                            |> Button.toHtml
            in
            if conversation.activeGeneration == Nothing then
                H.div [ A.css [ S.row, S.g2, S.flexWrap ] ]
                    [ Button.primary "Send" SendButtonClicked
                        |> Button.toHtml
                    , Button.secondary "Let selected person reply"
                        ReplyButtonClicked
                        |> Button.toHtml
                    , Button.secondary "Run 6 turns" RunButtonClicked
                        |> Button.toHtml
                    , autonomyButton
                    ]

            else
                H.p [ A.attribute "role" "status" ]
                    [ H.text
                        "Waiting for a reply… The Haskell worker must be running."
                    ]
    in
    H.fieldset
        [ A.disabled model.pending
        , A.css [ Css.border (Css.px 0), S.col, S.g2 ]
        ]
        [ H.label [ A.css [ S.col, S.g2 ] ]
            [ H.text "Message"
            , Textarea.simple model.draft DraftInputChanged |> Textarea.toHtml
            ]
        , H.div
            [ A.id "speaker-feedback"
            , A.attribute "role" "alert"
            ]
            [ speakerFeedback ]
        , replyControls
        , Button.secondary "Stop" StopButtonClicked |> Button.toHtml
        ]


noteHistoryView : Model -> Html Msg
noteHistoryView model =
    let
        loadLabel : String
        loadLabel =
            if model.loadingNotes then
                "Loading note history…"

            else
                "Load / refresh note history"

        history : Html Msg
        history =
            let
                revisionView : Chat.NoteRevision -> Html Msg
                revisionView revision =
                    H.details [ A.css [ S.col, S.g2 ] ]
                        [ H.summary []
                            [ H.text
                                ("Turn "
                                    ++ GenerationIdUtil.toString revision.generationId
                                    ++ " — "
                                    ++ personName model revision.author
                                )
                            ]
                        , pre (NoteUtil.toString revision.content)
                        ]
            in
            case model.noteHistory of
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
                            |> List.map revisionView
                        )
    in
    H.section [ A.css [ S.col, S.g2 ] ]
        [ H.h2 [ A.css [ S.textGray3 ] ] [ H.text "Note history" ]
        , H.fieldset
            [ A.disabled model.loadingNotes
            , A.css [ Css.border (Css.px 0), S.minW0 ]
            ]
            [ Button.secondary loadLabel NoteHistoryClicked |> Button.toHtml ]
        , history
        ]


personName : Model -> PersonId -> String
personName model id =
    model.people
        |> List.filter (\p -> p.id == id)
        |> List.head
        |> Maybe.map .name
        |> Maybe.withDefault "Unknown person"


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


generationView : Model -> Generation.GenerationSummary -> Html Msg
generationView model generation =
    let
        phaseName : String
        phaseName =
            case generation.phase of
                Generation.Pending ->
                    "Queued"

                Generation.Running ->
                    "Generating"

                Generation.Completed ->
                    "Complete"

                Generation.Failed ->
                    "Failed"

                Generation.Cancelled ->
                    "Stopped"

        promptInspection : Html Msg
        promptInspection =
            case Dict.get (GenerationIdUtil.toString generation.id) model.prompts of
                Nothing ->
                    H.text ""

                Just "" ->
                    H.p [] [ H.text "No prompt has been saved for this turn yet." ]

                Just prompt ->
                    PromptInspection.view prompt
    in
    H.details [ A.css [ S.col, S.g2 ] ]
        [ H.summary []
            [ H.text
                ("Turn "
                    ++ GenerationIdUtil.toString generation.id
                    ++ " — "
                    ++ phaseName
                )
            ]
        , H.p [] [ H.text (GenerationErrorUtil.toString generation.error) ]
        , H.fieldset
            [ A.disabled
                (Set.member
                    (GenerationIdUtil.toString generation.id)
                    model.loadingPrompts
                )
            , A.css [ Css.border (Css.px 0), S.minW0 ]
            ]
            [ Button.secondary "Load / refresh prompt"
                (PromptInspectionClicked generation.id)
                |> Button.toHtml
            ]
        , promptInspection
        ]



----------------------------------------------------------------
-- SUBSCRIPTIONS
----------------------------------------------------------------


subscriptions : Sub Msg
subscriptions =
    Time.every 1500 (\_ -> TickReceived)
