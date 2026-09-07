module PersonPage exposing
    ( Model
    , Msg
    , document
    , init
    , setShared
    , shared
    , update
    )

import Acadia.Transaction exposing (Transaction)
import Css
import Document exposing (Document)
import Effect as E exposing (Eff)
import GenerationId.Util as GenerationIdUtil
import Goal
import GoalDescription
import GoalDescription.Util as GoalDescriptionUtil
import GoalId exposing (GoalId)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Memory
import MemoryContent
import MemoryContent.Util as MemoryContentUtil
import MemoryId exposing (MemoryId)
import MemoryKeywords
import MemoryKeywords.Util as MemoryKeywordsUtil
import Origin
import Person exposing (Person, PersonPageFlags)
import PersonId exposing (PersonId)
import PersonId.Util as PersonIdUtil
import Shared
import Style as S
import View.Button
import View.TextField as TextField
import View.Textarea


type alias Model =
    { shared : Shared.Model
    , person : Person
    , identity : String
    , aspirations : String
    , saving : Bool
    , status : String
    , goals : Maybe (List Goal.Goal)
    , memories : Maybe (List Memory.Memory)
    , goalDraft : String
    , memoryDraft : String
    , keywords : String
    , pending : Bool
    , mindStatus : String
    }


type Msg
    = IdentityInputChanged String
    | AspirationsInputChanged String
    | SaveButtonClicked
    | IdentityResponseReceived PersonId (Maybe PersonPageFlags)
    | GoalsResponseReceived PersonId (Maybe (List Goal.Goal))
    | MemoriesResponseReceived PersonId (Maybe (List Memory.Memory))
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


init : Shared.Model -> PersonPageFlags -> ( Model, Eff Msg )
init sharedModel flags =
    ( { shared = sharedModel
      , person = flags.person
      , identity = flags.person.identity
      , aspirations = flags.person.aspirations
      , saving = False
      , status = ""
      , goals = Nothing
      , memories = Nothing
      , goalDraft = ""
      , memoryDraft = ""
      , keywords = ""
      , pending = False
      , mindStatus = ""
      }
    , refresh flags.person.id
    )


shared : Model -> Shared.Model
shared model =
    model.shared


setShared : Shared.Model -> Model -> Model
setShared sharedModel model =
    { model | shared = sharedModel }


update : Msg -> Model -> ( Model, Eff Msg )
update msg model =
    case msg of
        IdentityInputChanged value ->
            ( { model | identity = value }, E.none )

        AspirationsInputChanged value ->
            ( { model | aspirations = value }, E.none )

        SaveButtonClicked ->
            if model.saving then
                ( model, E.none )

            else
                ( { model | saving = True, status = "Saving…" }
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
                            , saving = False
                            , status =
                                "Saved."
                          }
                        , E.none
                        )

                    Nothing ->
                        ( { model
                            | saving = False
                            , status =
                                "Could not save. This person may have changed elsewhere. Your "
                                    ++ "draft is still here; reload to get the latest saved version."
                          }
                        , E.none
                        )

        GoalsResponseReceived personId (Just goals) ->
            if personId /= model.person.id then
                ( model, E.none )

            else
                ( { model | goals = Just goals }, E.none )

        MemoriesResponseReceived personId (Just memories) ->
            if personId /= model.person.id then
                ( model, E.none )

            else
                ( { model | memories = Just memories }, E.none )

        GoalsResponseReceived personId Nothing ->
            if personId /= model.person.id then
                ( model, E.none )

            else
                ( { model | mindStatus = "Could not load goals. Try Refresh." }, E.none )

        MemoriesResponseReceived personId Nothing ->
            if personId /= model.person.id then
                ( model, E.none )

            else
                ( { model | mindStatus = "Could not load memories. Try Refresh." }, E.none )

        GoalInputChanged value ->
            ( { model | goalDraft = value }, E.none )

        MemoryInputChanged value ->
            ( { model | memoryDraft = value }, E.none )

        KeywordsInputChanged value ->
            ( { model | keywords = value }, E.none )

        AddGoalClicked ->
            if String.isEmpty (String.trim model.goalDraft) then
                ( { model | mindStatus = "Enter a goal first." }, E.none )

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
                ( { model | mindStatus = "Enter a memory first." }, E.none )

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
                    , mindStatus = "Saved."
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
                    , mindStatus =
                        "Could not save. Your draft is still here. Refresh and retry."
                  }
                , E.none
                )

        RefreshClicked ->
            ( { model | mindStatus = "" }, refresh model.person.id )


document : Model -> Document Msg
document model =
    { title = model.person.name
    , body = [ view model ]
    }


view : Model -> Html Msg
view model =
    H.div
        [ A.css
            [ S.minHFullViewport
            , Css.alignItems Css.flexStart
            , S.justifyCenter
            , S.p4
            , S.row
            , S.wFull
            ]
        ]
        [ H.article
            [ A.css
                [ S.bgGray1
                , S.col
                , S.g3
                , S.outdent
                , S.p3
                , S.wFull
                , S.minW0
                , Css.property "overflow-wrap" "anywhere"
                ]
            ]
            [ H.h1
                [ A.css
                    [ S.textGray3 ]
                ]
                [ H.text model.person.name ]
            , H.dl
                [ A.css
                    [ S.col, S.g2 ]
                ]
                [ detail "Name" model.person.name
                , detail "ID" (PersonIdUtil.toString model.person.id)
                ]
            , identityField "Identity"
                "What matters to this person and how they communicate."
                model.identity
                IdentityInputChanged
            , identityField "Aspirations"
                "What this person wants to explore or accomplish. These guide future goals."
                model.aspirations
                AspirationsInputChanged
            , View.Button.primary
                (if model.saving then
                    "Saving…"

                 else
                    "Save identity"
                )
                SaveButtonClicked
                |> View.Button.toHtml
            , H.p [ A.attribute "role" "status" ] [ H.text model.status ]
            , mindView model
            ]
        ]


detail : String -> String -> H.Html msg
detail label value =
    H.div [ A.css [ S.g2, S.row, S.flexWrap ] ]
        [ H.dt [ A.css [ S.textGray3 ] ] [ H.text label ]
        , H.dd [ A.css [ S.textGray4 ] ] [ H.text value ]
        ]


identityField : String -> String -> String -> (String -> Msg) -> H.Html Msg
identityField label help value onInput =
    H.label [ A.css [ S.col, S.g2 ] ]
        [ H.span [ A.css [ S.textGray3 ] ] [ H.text label ]
        , H.span [] [ H.text help ]
        , H.div [ A.css [ Css.height (Css.rem 7) ] ]
            [ View.Textarea.simple value onInput |> View.Textarea.toHtml ]
        ]


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
        ( { model | pending = True, mindStatus = "Saving…" }
        , E.attempt (MutationResponseReceived model.person.id mutation) transaction
        )


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
        , H.p [ A.attribute "role" "status" ] [ H.text model.mindStatus ]
        , H.h2 [ A.css [ S.textGray3 ] ] [ H.text "Goals" ]
        , rows "Loading goals…" "No goals yet." (List.map (goalView model)) model.goals
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
        , rows "Loading memories…" "No memories yet." (List.map (memoryView model)) model.memories
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


rows : String -> String -> (List a -> List (Html msg)) -> Maybe (List a) -> Html msg
rows loading empty render values =
    H.div [ A.css [ S.col, S.g3 ] ]
        (case values of
            Nothing ->
                [ H.text loading ]

            Just [] ->
                [ H.text empty ]

            Just items ->
                render items
        )


goalView : Model -> Goal.Goal -> Html Msg
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


memoryView : Model -> Memory.Memory -> Html Msg
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
button model label event =
    View.Button.secondary label event
        |> View.Button.toHtml
