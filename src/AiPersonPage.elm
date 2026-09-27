module AiPersonPage exposing
    ( Flags
    , Model
    , Msg
    , document
    , init
    , setShared
    , shared
    , update
    )

import Acadia.Transaction exposing (Transaction)
import AiPersonProfile exposing (AiPersonProfile)
import Css
import Document exposing (Document)
import Effect as E exposing (Eff)
import GenerationId.Util as GenerationIdUtil
import Goal exposing (Goal)
import GoalDescription
import GoalDescription.Util as GoalDescriptionUtil
import GoalId exposing (GoalId)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Memory exposing (Memory)
import MemoryContent
import MemoryContent.Util as MemoryContentUtil
import MemoryId exposing (MemoryId)
import MemoryKeywords
import MemoryKeywords.Util as MemoryKeywordsUtil
import Origin
import Person exposing (Person, PersonPageFlags)
import PersonId exposing (PersonId)
import Shared
import Style as S
import View.Button
import View.PersonProfile as PersonProfile
import View.TextField as TextField
import View.Textarea



-----------------------------------------------------------------
-- TYPES --
-----------------------------------------------------------------


type alias Model =
    { shared : Shared.Model
    , person : Person
    , identity : String
    , aspirations : String
    , status : Status
    , goals : List Goal
    , memories : List Memory
    , goalDraft : String
    , memoryDraft : String
    , keywords : String
    , pending : Bool
    , mindStatus : MindStatus
    }


type Status
    = Idle
    | Saving
    | Saved
    | SaveFailed


type MindStatus
    = MindIdle
    | MindSaving
    | MindSaved
    | GoalsLoadFailed
    | MemoriesLoadFailed
    | GoalRequired
    | MemoryRequired
    | MindSaveFailed


type Msg
    = IdentityInputChanged String
    | AspirationsInputChanged String
    | SaveButtonClicked
    | IdentityResponseReceived PersonId (Maybe PersonPageFlags)
    | GoalsResponseReceived PersonId (Maybe (List Goal))
    | MemoriesResponseReceived PersonId (Maybe (List Memory))
    | GoalInputChanged String
    | MemoryInputChanged String
    | KeywordsInputChanged String
    | AddGoalClicked
    | AddMemoryClicked
    | GoalStatusClicked GoalId Goal.GoalStatus
    | RetireMemoryClicked MemoryId
    | MutationResponseReceived PersonId Mutation (Maybe ())
    | RefreshClicked


type Mutation
    = AddedGoal
    | AddedMemory
    | UpdatedRecord



-----------------------------------------------------------------
-- INIT --
-----------------------------------------------------------------


type alias Flags =
    { person : Person
    , profile : AiPersonProfile
    , goals : List Goal
    , memories : List Memory
    }


init : Shared.Model -> Flags -> Model
init sharedModel flags =
    { shared = sharedModel
    , person = flags.person
    , identity = flags.profile.identity
    , aspirations = flags.profile.aspirations
    , status = Idle
    , goals = flags.goals
    , memories = flags.memories
    , goalDraft = ""
    , memoryDraft = ""
    , keywords = ""
    , pending = False
    , mindStatus = MindIdle
    }



-----------------------------------------------------------------
-- API --
-----------------------------------------------------------------


shared : Model -> Shared.Model
shared model =
    model.shared


setShared : Shared.Model -> Model -> Model
setShared sharedModel model =
    { model | shared = sharedModel }



-----------------------------------------------------------------
-- UPDATE --
-----------------------------------------------------------------


update : Msg -> Model -> ( Model, Eff Msg )
update msg model =
    case msg of
        IdentityInputChanged value ->
            ( { model | identity = value }, E.none )

        AspirationsInputChanged value ->
            ( { model | aspirations = value }, E.none )

        SaveButtonClicked ->
            if model.status == Saving then
                ( model, E.none )

            else
                ( { model | status = Saving }
                , E.attempt (IdentityResponseReceived model.person.id)
                    (Person.updatePersonIdentity model.person.id
                        model.person.revision
                        model.identity
                        model.aspirations
                    )
                )

        IdentityResponseReceived personId response ->
            if personId /= model.person.id then
                ( model, E.none )

            else
                case response of
                    Just flags ->
                        ( { model
                            | person = flags.person
                            , status = Saved
                          }
                        , E.none
                        )

                    Nothing ->
                        ( { model
                            | status = SaveFailed
                          }
                        , E.none
                        )

        GoalsResponseReceived personId (Just goals) ->
            if personId /= model.person.id then
                ( model, E.none )

            else
                ( { model | goals = goals }, E.none )

        MemoriesResponseReceived personId (Just memories) ->
            if personId /= model.person.id then
                ( model, E.none )

            else
                ( { model | memories = memories }, E.none )

        GoalsResponseReceived personId Nothing ->
            if personId /= model.person.id then
                ( model, E.none )

            else
                ( { model | mindStatus = GoalsLoadFailed }, E.none )

        MemoriesResponseReceived personId Nothing ->
            if personId /= model.person.id then
                ( model, E.none )

            else
                ( { model | mindStatus = MemoriesLoadFailed }, E.none )

        GoalInputChanged value ->
            ( { model | goalDraft = value }, E.none )

        MemoryInputChanged value ->
            ( { model | memoryDraft = value }, E.none )

        KeywordsInputChanged value ->
            ( { model | keywords = value }, E.none )

        AddGoalClicked ->
            if String.isEmpty (String.trim model.goalDraft) then
                ( { model | mindStatus = GoalRequired }, E.none )

            else
                mutate AddedGoal
                    (Goal.createGoal model.person.id
                        (GoalDescription.GoalDescription
                            (String.trim model.goalDraft)
                        )
                    )
                    model

        AddMemoryClicked ->
            if String.isEmpty (String.trim model.memoryDraft) then
                ( { model | mindStatus = MemoryRequired }, E.none )

            else
                mutate AddedMemory
                    (Memory.createMemory model.person.id
                        (MemoryContent.MemoryContent (String.trim model.memoryDraft))
                        (MemoryKeywords.MemoryKeywords (String.trim model.keywords))
                    )
                    model

        GoalStatusClicked id status ->
            mutate UpdatedRecord (Goal.setGoalStatus model.person.id id status) model

        RetireMemoryClicked id ->
            mutate UpdatedRecord (Memory.retireMemory model.person.id id) model

        MutationResponseReceived personId mutation (Just ()) ->
            if personId /= model.person.id then
                ( model, E.none )

            else
                ( { model
                    | pending = False
                    , mindStatus = MindSaved
                    , goalDraft =
                        if mutation == AddedGoal then
                            ""

                        else
                            model.goalDraft
                    , memoryDraft =
                        if mutation == AddedMemory then
                            ""

                        else
                            model.memoryDraft
                    , keywords =
                        if mutation == AddedMemory then
                            ""

                        else
                            model.keywords
                  }
                , refresh model.person.id
                )

        MutationResponseReceived personId _ Nothing ->
            if personId /= model.person.id then
                ( model, E.none )

            else
                ( { model
                    | pending = False
                    , mindStatus = MindSaveFailed
                  }
                , E.none
                )

        RefreshClicked ->
            ( { model | mindStatus = MindIdle }, refresh model.person.id )



-----------------------------------------------------------------
-- HELPERS --
-----------------------------------------------------------------


refresh : PersonId -> Eff Msg
refresh personId =
    E.batch
        [ E.attempt (GoalsResponseReceived personId) (Goal.getGoals personId)
        , E.attempt (MemoriesResponseReceived personId) (Memory.getMemories personId)
        ]


mutate : Mutation -> Transaction () -> Model -> ( Model, Eff Msg )
mutate mutation transaction model =
    if model.pending then
        ( model, E.none )

    else
        ( { model | pending = True, mindStatus = MindSaving }
        , E.attempt (MutationResponseReceived model.person.id mutation) transaction
        )



-----------------------------------------------------------------
-- VIEW --
-----------------------------------------------------------------


document : Model -> Document Msg
document model =
    { title = model.person.name
    , body = [ view model ]
    }


view : Model -> Html Msg
view model =
    let
        saveLabel : String
        saveLabel =
            if model.status == Saving then
                "Saving…"

            else
                "Save identity"
    in
    PersonProfile.view model.person
        [ identityField "Identity"
            "What matters to this person and how they communicate."
            model.identity
            IdentityInputChanged
        , identityField "Aspirations"
            "What this person wants to explore or accomplish. These guide future goals."
            model.aspirations
            AspirationsInputChanged
        , View.Button.primary
            saveLabel
            SaveButtonClicked
            |> View.Button.toHtml
        , H.p [ A.attribute "role" "status" ] [ H.text (statusToString model.status) ]
        , mindView model
        ]


identityField : String -> String -> String -> (String -> Msg) -> H.Html Msg
identityField label help value onInput =
    H.label [ A.css [ S.col, S.g2 ] ]
        [ H.span [ A.css [ S.textGray3 ] ] [ H.text label ]
        , H.span [] [ H.text help ]
        , H.div [ A.css [ Css.height (Css.rem 7) ] ]
            [ View.Textarea.simple value onInput |> View.Textarea.toHtml ]
        ]


mindView : Model -> Html Msg
mindView model =
    H.fieldset
        [ A.disabled model.pending
        , A.css
            [ S.col
            , S.g3
            , Css.border (Css.px 0)
            , S.minW0
            , Css.property
                "overflow-wrap"
                "anywhere"
            ]
        ]
        [ H.legend [ A.css [ S.textGray3 ] ] [ H.text "Goals and memory" ]
        , H.p
            []
            [ H.text
                ("AI turns can create and complete goals and save reflections. "
                    ++ "Refresh to see their latest changes."
                )
            ]
        , button model "Refresh" RefreshClicked
        , H.p [ A.attribute "role" "status" ] [ H.text (mindStatusToString model.mindStatus) ]
        , H.h2 [ A.css [ S.textGray3 ] ] [ H.text "Goals" ]
        , rows "No goals yet." (List.map (goalView model)) model.goals
        , field
            "New goal"
            (View.Textarea.simple model.goalDraft GoalInputChanged
                |> View.Textarea.toHtml
            )
        , button model "Add goal" AddGoalClicked
        , H.h2 [ A.css [ S.textGray3 ] ] [ H.text "Memories" ]
        , H.p
            []
            [ H.text
                ("Reflections are AI recollections and may be mistaken. Retired "
                    ++ "memories remain visible here but are excluded from future "
                    ++ "prompts."
                )
            ]
        , rows "No memories yet." (List.map (memoryView model)) model.memories
        , field
            "New memory"
            (View.Textarea.simple model.memoryDraft MemoryInputChanged
                |> View.Textarea.toHtml
            )
        , field
            "Keywords (comma-separated; blank means always eligible)"
            (TextField.simple model.keywords KeywordsInputChanged
                |> TextField.toHtml
            )
        , button model "Add memory" AddMemoryClicked
        ]


rows : String -> (List a -> List (Html msg)) -> List a -> Html msg
rows empty render values =
    let
        content : List (Html msg)
        content =
            case values of
                [] ->
                    [ H.text empty ]

                items ->
                    render items
    in
    H.div [ A.css [ S.col, S.g3 ] ] content


goalView : Model -> Goal -> Html Msg
goalView model goal =
    H.div [ A.css [ S.col, S.g2 ] ]
        [ H.p [] [ H.text (GoalDescriptionUtil.toString goal.description) ]
        , H.p [] [ H.text (goalStatus goal.status ++ " · " ++ source goal.source) ]
        , case goal.status of
            Goal.Active ->
                H.div [ A.css [ S.row, S.g2 ] ]
                    [ button model "Complete" (GoalStatusClicked goal.id Goal.Completed)
                    , button model "Abandon" (GoalStatusClicked goal.id Goal.Abandoned)
                    ]

            _ ->
                button model "Reopen" (GoalStatusClicked goal.id Goal.Active)
        ]


memoryView : Model -> Memory -> Html Msg
memoryView model memory =
    H.div [ A.css [ S.col, S.g2 ] ]
        [ H.p [] [ H.text (MemoryContentUtil.toString memory.content) ]
        , H.p [] [ H.text (source memory.source) ]
        , H.p []
            [ H.text
                ("Keywords: "
                    ++ (if MemoryKeywordsUtil.toString memory.keywords == "" then
                            "always eligible"

                        else
                            MemoryKeywordsUtil.toString memory.keywords
                       )
                )
            ]
        , if memory.retired then
            H.p [] [ H.text "Retired" ]

          else
            button model "Retire memory" (RetireMemoryClicked memory.id)
        ]


goalStatus : Goal.GoalStatus -> String
goalStatus status =
    case status of
        Goal.Active ->
            "Active"

        Goal.Completed ->
            "Completed"

        Goal.Abandoned ->
            "Abandoned"


source : Origin.Origin -> String
source origin =
    case origin of
        Origin.Curated ->
            "Added by you"

        Origin.Reflection id ->
            "AI reflection from turn " ++ GenerationIdUtil.toString id


field : String -> Html msg -> Html msg
field label input =
    H.label [ A.css [ S.col, S.g2 ] ] [ H.span [] [ H.text label ], input ]


button : Model -> String -> Msg -> Html Msg
button model label msg =
    View.Button.secondary label msg
        |> View.Button.toHtml


statusToString : Status -> String
statusToString status =
    case status of
        Idle ->
            ""

        Saving ->
            "Saving…"

        Saved ->
            "Saved."

        SaveFailed ->
            "Could not save. This person may have changed elsewhere. Your "
                ++ "draft is still here; reload to get the latest saved version."


mindStatusToString : MindStatus -> String
mindStatusToString status =
    case status of
        MindIdle ->
            ""

        MindSaving ->
            "Saving…"

        MindSaved ->
            "Saved."

        GoalsLoadFailed ->
            "Could not load goals. Try Refresh."

        MemoriesLoadFailed ->
            "Could not load memories. Try Refresh."

        GoalRequired ->
            "Enter a goal first."

        MemoryRequired ->
            "Enter a memory first."

        MindSaveFailed ->
            "Could not save. Your draft is still here. Refresh and retry."
