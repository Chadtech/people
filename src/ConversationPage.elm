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
import List.Util as ListUtil
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
import TurnCount exposing (TurnCount)
import View.Button as Button
import View.Textarea as Textarea



----------------------------------------------------------------
-- TYPES
----------------------------------------------------------------


type alias Model =
    { shared : Shared.Model
    , conversation : Conversation
    , currentPerson : PersonId
    , people : List Person
    , messages : List (Attributed Message)
    , participants : List Person
    , noteHistory : Maybe (List (Attributed Chat.NoteRevision))
    , prompts : Dict String String
    , loadingPrompts : Set String
    , loadingNotes : Bool
    , generations : List Generation.GenerationSummary
    , draft : String
    , speaker : String
    , speakerError : Maybe SpeakerError
    , pending : Bool
    , error : Maybe Error
    }


type alias Attributed a =
    { value : a
    , author : Person
    }


type Error
    = ConversationRefreshFailed
    | ConversationUnavailable
    | PromptNotFound
    | PromptLoadFailed
    | NoteHistoryLoadFailed
    | ParticipantSelectionRequired
    | MutationFailed
    | MessageSendFailed
    | TurnStartFailed


type SpeakerError
    = AiPersonSpeakerRequired
    | SpeakerNotParticipant
    | SpeakerSelectionRequired
    | HumanPersonSpeakerRequired


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
    | RunButtonClicked TurnCount
    | AutonomyButtonClicked
    | StopButtonClicked
    | AddParticipantButtonClicked
    | MutationResponseReceived (Maybe ())
    | TurnResponseReceived (Maybe GenerationId)
    | MessageResponseReceived String (Maybe ())



----------------------------------------------------------------
-- INIT
----------------------------------------------------------------


type alias Flags =
    { conversation : Conversation
    , currentPerson : PersonId
    , people : List Person
    , messages : List (Attributed Message)
    , participants : List Person
    , generations : List GenerationSummary
    }


type alias PageFlags =
    { conversation : Maybe Conversation
    , currentPerson : Maybe PersonId
    , people : List Person
    , messages : List Message
    , participants : List PersonId
    , generations : List Generation.GenerationSummary
    }


init : Shared.Model -> Flags -> Model
init sharedModel flags =
    { shared = sharedModel
    , conversation = flags.conversation
    , currentPerson = flags.currentPerson
    , people = flags.people
    , messages = flags.messages
    , participants = flags.participants
    , generations = flags.generations
    , noteHistory = Nothing
    , loadingNotes = False
    , prompts = Dict.empty
    , loadingPrompts = Set.empty
    , draft = ""
    , speaker = PersonIdUtil.toString flags.currentPerson
    , speakerError = Nothing
    , pending = False
    , error = Nothing
    }


addPageFlag : Chat.ConversationPageFlag -> PageFlags -> PageFlags
addPageFlag flag flags =
    case flag of
        Chat.ConversationFlag conversation ->
            { flags | conversation = Just conversation }

        Chat.CurrentPersonFlag personId ->
            { flags | currentPerson = Just personId }

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


resolvePerson : List Person -> PersonId -> Maybe Person
resolvePerson people personId =
    case ListUtil.findUnique (\person -> person.id == personId) people of
        Just person ->
            if String.isEmpty (String.trim person.name) then
                Nothing

            else
                Just person

        _ ->
            Nothing


resolveAll : (a -> Maybe b) -> List a -> Maybe (List b)
resolveAll resolve =
    List.foldr (\value -> Maybe.map2 (::) (resolve value)) (Just [])


resolveAuthors : List Person -> List { a | author : PersonId } -> Maybe (List (Attributed { a | author : PersonId }))
resolveAuthors people =
    resolveAll (\value -> Maybe.map (Attributed value) (resolvePerson people value.author))


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
                        , currentPerson = Nothing
                        , people = []
                        , messages = []
                        , participants = []
                        , generations = []
                        }
                        rows
            in
            case ( flags.conversation, flags.currentPerson ) of
                ( Just conversation, Just currentPerson ) ->
                    let
                        resolved : Maybe Flags
                        resolved =
                            Maybe.map3
                                (\_ messages participants ->
                                    { conversation = conversation
                                    , currentPerson = currentPerson
                                    , people = flags.people
                                    , messages = messages
                                    , participants = participants
                                    , generations = flags.generations
                                    }
                                )
                                (resolveAll (resolvePerson flags.people)
                                    (currentPerson :: List.map .id flags.people)
                                )
                                (resolveAuthors flags.people flags.messages)
                                (resolveAll (resolvePerson flags.people) flags.participants)
                    in
                    case resolved of
                        Just validFlags ->
                            Remote.Found validFlags

                        Nothing ->
                            Remote.Failed

                ( Nothing, _ ) ->
                    Remote.NotFound

                ( Just _, Nothing ) ->
                    Remote.Failed


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


errorToString : Error -> String
errorToString error =
    case error of
        ConversationRefreshFailed ->
            "Could not refresh the conversation. Check the connection and person records."

        ConversationUnavailable ->
            "This conversation is no longer available. Return to All conversations."

        PromptNotFound ->
            "That prompt was not found."

        PromptLoadFailed ->
            "Could not load that prompt. Try again."

        NoteHistoryLoadFailed ->
            "Could not load note history. Try again."

        ParticipantSelectionRequired ->
            "Choose a person to add."

        MutationFailed ->
            "Could not apply the change. Refresh and try again."

        MessageSendFailed ->
            "Could not send your message. Your draft is still here; try again."

        TurnStartFailed ->
            "Could not start the turn. The conversation may have changed; your draft is still here."


speakerErrorToString : SpeakerError -> String
speakerErrorToString error =
    case error of
        AiPersonSpeakerRequired ->
            "Choose an AI person to request a reply."

        SpeakerNotParticipant ->
            "Add the selected person to the conversation, then try again."

        SpeakerSelectionRequired ->
            "Choose who should reply from the Person menu above, then try again."

        HumanPersonSpeakerRequired ->
            "Choose the human profile in the Person menu to send a message."


setError : Error -> Model -> Model
setError error model =
    { model | error = Just error }


clearError : Model -> Model
clearError model =
    { model | error = Nothing }


setSpeakerError : SpeakerError -> Model -> Model
setSpeakerError error model =
    { model | speakerError = Just error }


clearSpeakerError : Model -> Model
clearSpeakerError model =
    { model | speakerError = Nothing }


requestTurn : Model -> ( Model, Eff Msg )
requestTurn model =
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

            else if not (List.any (\p -> p.id == speaker && isAiPerson p) model.people) then
                model
                    |> setSpeakerError AiPersonSpeakerRequired
                    |> E.withOut

            else if not (List.any (\person -> person.id == speaker) model.participants) then
                model
                    |> setSpeakerError SpeakerNotParticipant
                    |> E.withOut

            else
                ( { model | pending = True }
                    |> clearError
                    |> clearSpeakerError
                , E.attempt TurnResponseReceived
                    (Chat.requestTurn model.conversation.id
                        model.conversation.revision
                        speaker
                    )
                )

        _ ->
            model
                |> setSpeakerError SpeakerSelectionRequired
                |> E.withOut



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
                    model
                        |> setError ConversationRefreshFailed
                        |> E.withOut

                Remote.NotFound ->
                    model
                        |> setError ConversationUnavailable
                        |> E.withOut

                Remote.Found flags ->
                    let
                        refreshedHistory : Maybe (List (Attributed Chat.NoteRevision))
                        refreshedHistory =
                            model.noteHistory
                                |> Maybe.andThen
                                    (List.map .value >> resolveAuthors flags.people)

                        refreshedModel : Model
                        refreshedModel =
                            { model
                                | conversation = flags.conversation
                                , people = flags.people
                                , messages = flags.messages
                                , participants = flags.participants
                                , generations = flags.generations
                                , noteHistory = refreshedHistory
                            }
                    in
                    if model.noteHistory /= Nothing && refreshedHistory == Nothing then
                        ( refreshedModel |> setError NoteHistoryLoadFailed, E.none )

                    else
                        ( refreshedModel, E.none )

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
                    next
                        |> setError PromptNotFound
                        |> E.withOut

                Remote.Failed ->
                    next
                        |> setError PromptLoadFailed
                        |> E.withOut

        NoteHistoryClicked ->
            if model.loadingNotes then
                ( model, E.none )

            else
                ( { model | loadingNotes = True }
                , E.attempt NoteHistoryResponseReceived (Chat.getNoteRevisions model.conversation.id)
                )

        NoteHistoryResponseReceived result ->
            case Maybe.andThen (resolveAuthors model.people) result of
                Just revisions ->
                    ( { model | loadingNotes = False, noteHistory = Just revisions }
                    , E.none
                    )

                Nothing ->
                    ( { model | loadingNotes = False }
                        |> setError NoteHistoryLoadFailed
                    , E.none
                    )

        DraftInputChanged value ->
            ( { model | draft = value }, E.none )

        SpeakerSelectionChanged value ->
            { model | speaker = value }
                |> clearSpeakerError
                |> E.withOut

        SendButtonClicked ->
            if model.pending || String.isEmpty (String.trim model.draft) then
                ( model, E.none )

            else if model.speaker == PersonIdUtil.toString model.currentPerson then
                ( { model | pending = True }
                    |> clearError
                    |> clearSpeakerError
                , E.attempt (MessageResponseReceived model.draft)
                    (Chat.sendMessage model.conversation.id
                        (MessageContent.MessageContent model.draft)
                    )
                )

            else
                model
                    |> setSpeakerError HumanPersonSpeakerRequired
                    |> E.withOut

        ReplyButtonClicked ->
            requestTurn model

        RunButtonClicked turns ->
            mutateConversation
                (\c ->
                    Conversation.setRunLength
                        c.id
                        c.revision
                        turns
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
                    model
                        |> setError ParticipantSelectionRequired
                        |> E.withOut

                Just personId ->
                    mutateConversation (\c -> Conversation.addParticipant c.id personId) model

        MutationResponseReceived result ->
            ( { model
                | pending = False
              }
                |> (if result == Nothing then
                        setError MutationFailed

                    else
                        clearError
                   )
            , refresh model.conversation.id
            )

        MessageResponseReceived submitted result ->
            ( { model
                | pending = False
                , draft =
                    if result /= Nothing && model.draft == submitted then
                        ""

                    else
                        model.draft
              }
                |> (if result == Nothing then
                        setError MessageSendFailed

                    else
                        clearError
                   )
            , refresh model.conversation.id
            )

        TurnResponseReceived result ->
            ( { model
                | pending = False
              }
                |> (if result == Nothing then
                        setError TurnStartFailed

                    else
                        clearError
                   )
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
            [ conversationView model model.conversation ]
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
                messageView : Attributed Message -> Html Msg
                messageView message =
                    H.section [ A.css [ S.col, S.g2 ] ]
                        [ H.h3 [ A.css [ S.textGray3 ] ]
                            [ H.text
                                message.author.name
                            ]
                        , pre (MessageContentUtil.toString message.value.content)
                        ]
            in
            H.div
                [ A.css [ S.col, S.g3, S.flex1, S.minH0, S.overflowAuto, S.p3, S.indent, S.bgNightwood1 ]
                , A.tabindex 0
                , A.attribute "role" "region"
                , A.attribute "aria-label" "Conversation messages"
                ]
                (model.messages
                    |> List.sortBy (\m -> String.padLeft 20 '0' (MessageIdUtil.toString m.value.id))
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
                        :: List.map personOption
                            (List.filter (\p -> p.id == model.currentPerson || isAiPerson p) model.people)
                    )
                ]

        controls : Html Msg
        controls =
            H.section
                [ A.css
                    [ S.col
                    , S.g3
                    , S.p3
                    , S.bgGray1
                    , S.outdent
                    , S.minW0
                    , S.wrapAnywhere
                    , S.overflowAuto
                    , Css.maxHeight (Css.vh 60)
                    , S.lg [ Css.width (Css.rem 20), S.flex00auto, Css.maxHeight Css.none ]
                    ]
                , A.attribute "aria-label" "Conversation controls"
                ]
                [ H.a
                    [ Route.href Route.Conversations, A.css [ S.link ] ]
                    [ H.text "All conversations" ]
                , H.h1 [ A.css [ S.textGray3 ] ] [ H.text "Conversation" ]
                , H.p []
                    [ H.text
                        ("Participants: "
                            ++ String.join ", " (List.map .name model.participants)
                        )
                    ]
                , H.p [ A.attribute "role" "status" ] [ H.text autonomyStatus ]
                , H.fieldset
                    [ A.disabled model.pending
                    , A.css [ S.border0, S.col, S.g2, S.minW0 ]
                    ]
                    [ personSelector
                    , Button.secondary "Add participant" AddParticipantButtonClicked
                        |> Button.toHtml
                    ]
                , H.a
                    [ Route.href (Route.Person model.currentPerson), A.css [ S.link ] ]
                    [ H.text "Edit human profile" ]
                , replyControlsView model conversation
                , sharedNote
                , noteHistoryView model
                , generationHistory
                ]

        errorStatus : Html Msg
        errorStatus =
            case model.error of
                Nothing ->
                    H.text ""

                Just error ->
                    H.p [ A.attribute "role" "status", A.css [ S.wrapAnywhere ] ]
                        [ H.text (errorToString error) ]
    in
    H.div
        [ A.css
            [ S.col
            , S.g3
            , S.minW0
            , S.lg [ S.row, Css.property "height" "calc(100dvh - 2rem)" ]
            ]
        ]
        [ controls
        , H.article
            [ A.css
                [ S.col
                , S.g3
                , S.p3
                , S.bgGray1
                , S.outdent
                , S.minW0
                , S.h75Viewport
                , S.minH96
                , S.lg [ S.flex1, S.hFull, S.minH0 ]
                ]
            ]
            [ H.h2 [ A.css [ S.textGray3, S.wrapAnywhere ] ]
                [ H.text (ConversationTitleUtil.toString conversation.title) ]
            , messages
            , composerView model
            , errorStatus
            ]
        ]


composerView : Model -> Html Msg
composerView model =
    let
        speakingAsMyself : Bool
        speakingAsMyself =
            model.speaker == PersonIdUtil.toString model.currentPerson

        messageInput : Html Msg
        messageInput =
            if speakingAsMyself then
                H.div [ A.css [ S.col, S.g2 ] ]
                    [ H.label [ A.css [ S.col, S.g2 ] ]
                        [ H.text "Message"
                        , H.div [ A.css [ S.h16 ] ]
                            [ Textarea.simple model.draft DraftInputChanged |> Textarea.toHtml ]
                        ]
                    , Button.primary "Send" SendButtonClicked |> Button.toHtml
                    ]

            else
                H.text ""
    in
    H.fieldset
        [ A.disabled model.pending
        , A.css [ S.border0, S.col, S.g2, S.minW0, S.flex00auto ]
        ]
        [ messageInput ]


replyControlsView : Model -> Conversation -> Html Msg
replyControlsView model conversation =
    let
        speakerFeedback : Html Msg
        speakerFeedback =
            case model.speakerError of
                Nothing ->
                    H.text ""

                Just error ->
                    H.p
                        [ A.css [ S.textRed1 ] ]
                        [ H.text (speakerErrorToString error) ]

        speakingAsMyself : Bool
        speakingAsMyself =
            model.speaker == PersonIdUtil.toString model.currentPerson

        selectedReply : Html Msg
        selectedReply =
            if speakingAsMyself then
                H.text ""

            else
                Button.primary
                    "Let selected person reply"
                    ReplyButtonClicked
                    |> Button.toHtml

        replyControls : Html Msg
        replyControls =
            let
                runButton : String -> Int -> Html Msg
                runButton label turns =
                    Button.secondary label
                        (RunButtonClicked
                            (TurnCount.TurnCount (Acadia.UInt8.fromInt turns))
                        )
                        |> Button.toHtml

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
                H.div
                    [ A.css
                        [ S.col
                        , S.g2
                        ]
                    ]
                    [ selectedReply
                    , runButton "Run 1 turn" 1
                    , runButton "Run 6 turns" 6
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
        , A.css [ S.border0, S.col, S.g2 ]
        ]
        [ H.div
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
                revisionView : Attributed Chat.NoteRevision -> Html Msg
                revisionView revision =
                    H.details [ A.css [ S.col, S.g2 ] ]
                        [ H.summary []
                            [ H.text
                                ("Turn "
                                    ++ GenerationIdUtil.toString revision.value.generationId
                                    ++ " — "
                                    ++ revision.author.name
                                )
                            ]
                        , pre (NoteUtil.toString revision.value.content)
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
                                        (GenerationIdUtil.toString revision.value.generationId)
                                )
                            |> List.reverse
                            |> List.map revisionView
                        )
    in
    H.section [ A.css [ S.col, S.g2 ] ]
        [ H.h2 [ A.css [ S.textGray3 ] ] [ H.text "Note history" ]
        , H.fieldset
            [ A.disabled model.loadingNotes
            , A.css [ S.border0, S.minW0 ]
            ]
            [ Button.secondary loadLabel NoteHistoryClicked |> Button.toHtml ]
        , history
        ]


pre : String -> Html msg
pre content =
    H.pre
        [ A.css
            [ S.whitespacePreWrap
            , S.wrapAnywhere
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
            , A.css [ S.border0, S.minW0 ]
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


isAiPerson : Person -> Bool
isAiPerson person =
    case person.kind of
        Person.AiPerson _ ->
            True

        Person.HumanPerson ->
            False
