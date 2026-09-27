module HumanPersonPage exposing
    ( Model
    , Msg
    , document
    , init
    , setShared
    , shared
    , update
    )

import Document exposing (Document)
import Effect as E exposing (Eff)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Person exposing (Person, PersonPageFlags)
import PersonId exposing (PersonId)
import Shared
import Style as S
import View.Button
import View.PersonProfile as PersonProfile
import View.TextField as TextField



-----------------------------------------------------------------
-- TYPES --
-----------------------------------------------------------------


type alias Model =
    { shared : Shared.Model
    , person : Person
    , name : String
    , status : Status
    }


type Status
    = Idle
    | NameRequired
    | Saving
    | Saved
    | SaveFailed


type Msg
    = NameInputChanged String
    | NameSaveButtonClicked
    | NameResponseReceived PersonId (Maybe PersonPageFlags)



-----------------------------------------------------------------
-- INIT --
-----------------------------------------------------------------


init : Shared.Model -> Person -> Model
init sharedModel person =
    { shared = sharedModel
    , person = person
    , name = person.name
    , status = Idle
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
        NameInputChanged value ->
            ( { model | name = value, status = Idle }, E.none )

        NameSaveButtonClicked ->
            if model.status == Saving then
                ( model, E.none )

            else if String.isEmpty (String.trim model.name) then
                ( { model | status = NameRequired }, E.none )

            else
                ( { model | status = Saving }
                , E.attempt (NameResponseReceived model.person.id)
                    (Person.updateHumanPersonName model.person.id
                        model.person.revision
                        (String.trim model.name)
                    )
                )

        NameResponseReceived personId response ->
            if personId /= model.person.id then
                ( model, E.none )

            else
                case response of
                    Just flags ->
                        ( { model
                            | person = flags.person
                            , name = flags.person.name
                            , status = Saved
                          }
                        , E.none
                        )

                    Nothing ->
                        ( { model | status = SaveFailed }
                        , E.none
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
        statusMessage : Maybe String
        statusMessage =
            case model.status of
                Idle ->
                    Nothing

                NameRequired ->
                    Just "Enter a name."

                Saving ->
                    Just "Saving…"

                Saved ->
                    Just "Saved."

                SaveFailed ->
                    Just
                        ("Could not save. The profile may have changed elsewhere. "
                            ++ "The draft is still here; reload to get the latest saved version."
                        )

        status : List (Html Msg)
        status =
            case statusMessage of
                Nothing ->
                    []

                Just message ->
                    [ H.p [ A.attribute "role" "status" ] [ H.text message ] ]
    in
    PersonProfile.view model.person
        ([ H.p [] [ H.text "Human profile" ]
         , H.fieldset
            [ A.disabled (model.status == Saving), A.css [ S.border0, S.col, S.g3, S.minW0 ] ]
            [ H.label [ A.css [ S.col, S.g2 ] ]
                [ H.span [] [ H.text "Name" ]
                , TextField.simple model.name NameInputChanged |> TextField.toHtml
                ]
            , H.p [] [ H.text "This name identifies messages in conversations and AI prompts." ]
            , View.Button.primary "Save name" NameSaveButtonClicked
                |> View.Button.toHtml
            ]
         ]
            ++ status
        )
