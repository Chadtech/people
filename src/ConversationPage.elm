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
import AssocList as Dict exposing (Dict)
import AssocSet as Set exposing (Set)
import Chat exposing (ConversationPageFlag, Message)
import Conversation exposing (Conversation)
import ConversationId exposing (ConversationId)
import ConversationTitle.Util as ConversationTitleUtil
import Css
import Document exposing (Document)
import Effect as E exposing (Eff)
import Generation exposing (GenerationSummary)
import GenerationError.Util as GenerationErrorUtil
import GenerationId exposing (GenerationId)
import GenerationId.Util as GenerationIdUtil
import Html.Styled as H exposing (Attribute, Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as Ev
import IntervalSeconds
import IntervalSeconds.Util as IntervalSecondsUtil
import List.Util as ListUtil
import MessageContent
import MessageContent.Util as MessageContentUtil
import MessageId.Util as MessageIdUtil
import Person exposing (Person)
import PersonId exposing (PersonId)
import PersonId.Util as PersonIdUtil
import PromptInspection
import PromptSnapshot exposing (PromptSnapshot)
import PromptSnapshot.Util as PromptSnapshotUtil
import Remote exposing (Remote)
import Route
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
    , prompts : Dict GenerationId PromptSnapshot
    , loadingPrompts : Set GenerationId
    , generations : List GenerationSummary
    , selectedView : ConversationView
    , draft : String
    , participantToAdd : String
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
    | ParticipantSelectionRequired
    | MutationFailed
    | MessageSendFailed
    | TurnStartFailed


type ConversationView
    = MessagesView
    | HistoryView


type SpeakerError
    = AiPersonSpeakerRequired
    | SpeakerNotParticipant
    | SpeakerSelectionRequired
    | HumanPersonSpeakerRequired


type Msg
    = ViewButtonClicked ConversationView
    | TickReceived
    | PageFlagsResponseReceived (Remote Flags)
    | PromptInspectionClicked GenerationId
    | PromptResponseReceived GenerationId (Remote PromptSnapshot)
    | DraftInputChanged String
    | ParticipantSelectionChanged String
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
    , prompts = Dict.empty
    , loadingPrompts = Set.empty
    , selectedView = MessagesView
    , draft = ""
    , participantToAdd = ""
    , speaker =
        flags.participants
            |> List.filter (\p -> p.id == flags.currentPerson || isAiPerson p)
            |> List.partition (\p -> p.id == flags.currentPerson)
            |> (\( local, others ) -> local ++ others)
            |> List.head
            |> Maybe.map (.id >> PersonIdUtil.toString)
            |> Maybe.withDefault ""
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
        ViewButtonClicked selectedView ->
            { model | selectedView = selectedView } |> E.withOut

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
                        refreshedModel : Model
                        refreshedModel =
                            { model
                                | conversation = flags.conversation
                                , people = flags.people
                                , messages = flags.messages
                                , participants = flags.participants
                                , participantToAdd =
                                    if List.any (\person -> PersonIdUtil.toString person.id == model.participantToAdd) flags.participants then
                                        ""

                                    else
                                        model.participantToAdd
                                , generations = flags.generations
                            }
                    in
                    ( refreshedModel, E.none )

        PromptInspectionClicked generationId ->
            if Set.member generationId model.loadingPrompts then
                ( model, E.none )

            else
                ( { model
                    | loadingPrompts =
                        Set.insert
                            generationId
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
                                generationId
                                model.loadingPrompts
                    }
            in
            case result of
                Remote.Found prompt ->
                    ( { next
                        | prompts =
                            Dict.insert
                                generationId
                                prompt
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

        DraftInputChanged value ->
            ( { model | draft = value }, E.none )

        ParticipantSelectionChanged value ->
            ( { model | participantToAdd = value }, E.none )

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
            case PersonIdUtil.fromString model.participantToAdd of
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
                [ S.p4, S.col, S.g3, S.wFull, S.hFull ]
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
            let
                generations : List (Html Msg)
                generations =
                    if List.isEmpty model.generations then
                        [ H.p [] [ H.text "No generations yet." ] ]

                    else
                        model.generations
                            |> List.sortBy (\g -> String.padLeft 20 '0' (GenerationIdUtil.toString g.id))
                            |> List.reverse
                            |> List.map (generationView model)
            in
            H.section [ A.css [ S.col, S.g2 ] ]
                [ H.h2 [ A.css [ S.textGray3 ] ] [ H.text "Generation history" ]
                , H.div [] generations
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
                            (List.filter (\p -> p.id == model.currentPerson || isAiPerson p) model.participants)
                    )
                ]

        participantSelector : Html Msg
        participantSelector =
            let
                availablePeople : List Person
                availablePeople =
                    List.filter
                        (\person -> not (List.any (\member -> member.id == person.id) model.participants))
                        model.people

                personOption : Person -> Html Msg
                personOption person =
                    H.option
                        [ A.value (PersonIdUtil.toString person.id)
                        , A.selected (model.participantToAdd == PersonIdUtil.toString person.id)
                        ]
                        [ H.text person.name ]
            in
            H.label [ A.css [ S.col, S.g2 ] ]
                [ H.text "Participant to add"
                , H.select
                    [ Ev.onInput ParticipantSelectionChanged, A.css [ S.selectControl ] ]
                    (H.option [ A.value "", A.selected (model.participantToAdd == "") ] [ H.text "Choose a person to add" ]
                        :: List.map personOption availablePeople
                    )
                ]

        controls : Html Msg
        controls =
            let
                autonomyControl : Html Msg
                autonomyControl =
                    if conversation.autonomous then
                        H.text ""

                    else
                        H.fieldset
                            [ A.disabled (model.pending || conversation.activeGeneration /= Nothing)
                            , A.css [ S.border0, S.minW0 ]
                            ]
                            [ Button.secondary "Enable autonomy every 5 minutes"
                                AutonomyButtonClicked
                                |> Button.toHtml
                            ]

                controlsNeeded : Bool
                controlsNeeded =
                    conversation.autonomous

                openAttributes : List (Attribute Msg)
                openAttributes =
                    if controlsNeeded then
                        [ A.attribute "open" "" ]

                    else
                        []
            in
            H.details
                (A.css [ S.flex00auto ] :: openAttributes)
                [ H.summary [ A.css [ S.pointerCursor ] ]
                    [ H.text "Participants and automation" ]
                , H.div [ A.css [ S.col, S.g2, S.py2 ] ]
                    [ H.p [ A.attribute "role" "status" ] [ H.text autonomyStatus ]
                    , H.fieldset
                        [ A.disabled model.pending
                        , A.css [ S.border0, S.minW0, S.col, S.g2 ]
                        ]
                        [ participantSelector
                        , Button.secondary "Add participant" AddParticipantButtonClicked
                            |> Button.toHtml
                        ]
                    , H.a
                        [ Route.href (Route.Person model.currentPerson), A.css [ S.link ] ]
                        [ H.text "Edit human profile" ]
                    , autonomyControl
                    ]
                ]

        viewSelector : Html Msg
        viewSelector =
            let
                viewButton : ConversationView -> String -> Html Msg
                viewButton selectedView label =
                    let
                        selected : Bool
                        selected =
                            model.selectedView == selectedView

                        pressed : String
                        pressed =
                            if selected then
                                "true"

                            else
                                "false"

                        appearance : Css.Style
                        appearance =
                            if selected then
                                S.batch [ S.indent, S.bgGray1, S.textGray5 ]

                            else
                                S.batch [ S.outdent, S.bgGray1, S.textGray4 ]
                    in
                    H.button
                        [ A.type_ "button"
                        , A.attribute "aria-pressed" pressed
                        , A.attribute "aria-controls" "conversation-content"
                        , Ev.onClick (ViewButtonClicked selectedView)
                        , A.css [ S.p2, S.pointerCursor, appearance ]
                        ]
                        [ H.text label ]
            in
            H.div
                [ A.attribute "role" "group"
                , A.attribute "aria-label" "Conversation views"
                , A.css [ S.row, S.flexWrap, S.g2 ]
                ]
                [ viewButton MessagesView "Conversation"
                , viewButton HistoryView "Generation history"
                ]

        selectedContent : Html Msg
        selectedContent =
            case model.selectedView of
                MessagesView ->
                    H.div
                        [ A.css [ S.col, S.g3, S.flex1, S.minH0 ] ]
                        [ H.div
                            [ A.css [ S.col, S.g3, S.flex1, S.minH96 ] ]
                            [ messages
                            , personSelector
                            , composerView model
                            , replyControlsView model conversation
                            ]
                        , controls
                        ]

                HistoryView ->
                    generationHistory

        errorStatus : Html Msg
        errorStatus =
            case model.error of
                Nothing ->
                    H.text ""

                Just error ->
                    H.p [ A.attribute "role" "status", A.css [ S.wrapAnywhere ] ]
                        [ H.text (errorToString error) ]
    in
    H.article
        [ A.css [ S.col, S.g3, S.p3, S.bgGray1, S.outdent, S.minW0, S.wrapAnywhere, S.flex1, S.minH0, S.overflowAuto ] ]
        [ H.a
            [ Route.href Route.Conversations, A.css [ S.link ] ]
            [ H.text "All conversations" ]
        , H.h1 [ A.css [ S.textGray3 ] ]
            [ H.text (ConversationTitleUtil.toString conversation.title) ]
        , H.p []
            [ H.text ("Participants: " ++ String.join ", " (List.map .name model.participants)) ]
        , viewSelector
        , H.div
            [ A.id "conversation-content", A.css [ S.col, S.flex1, S.minH0 ] ]
            [ selectedContent ]
        , errorStatus
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
            in
            if conversation.activeGeneration == Nothing then
                H.div
                    [ A.css
                        [ S.row
                        , S.flexWrap
                        , S.g2
                        ]
                    ]
                    [ selectedReply
                    , runButton "Run 1 turn" 1
                    , runButton "Run 6 turns" 6
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
            case Dict.get generation.id model.prompts of
                Nothing ->
                    H.text ""

                Just prompt ->
                    let
                        raw : String
                        raw =
                            PromptSnapshotUtil.toString prompt
                    in
                    if String.isEmpty raw then
                        H.p [] [ H.text "No prompt has been saved for this turn yet." ]

                    else
                        PromptInspection.view raw
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
                    generation.id
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
