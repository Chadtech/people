module NewPerson exposing
    ( Model
    , Msg
    , document
    , init
    , setShared
    , shared
    , update
    )

import Person
import Css
import Document exposing (Document)
import Effect as E exposing (Eff)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as Event
import PersonId exposing (PersonId)
import Route
import Shared
import Style as S
import View.Button as Button
import View.TextField as TextField


type alias Model =
    { shared : Shared.Model
    , newPerson : String
    , status : Status
    }


type Status
    = Ready
    | Saving
    | Created PersonId
    | Failed String


type Msg
    = UpdatedNewPersonNameField String
    | SubmittedPerson
    | CreatedNewPerson (Maybe PersonId)


init : Shared.Model -> Model
init sharedModel =
    { shared = sharedModel
    , newPerson = ""
    , status = Ready
    }


shared : Model -> Shared.Model
shared model =
    model.shared


setShared : Shared.Model -> Model -> Model
setShared sharedModel model =
    { model | shared = sharedModel }


update : Msg -> Model -> ( Model, Eff Msg )
update msg model =
    case msg of
        UpdatedNewPersonNameField newPerson ->
            ( { model | newPerson = newPerson }
            , E.none
            )

        SubmittedPerson ->
            let
                personName : String
                personName =
                    String.trim model.newPerson
            in
            if String.isEmpty personName || model.status == Saving then
                ( model, E.none )

            else
                ( { model | status = Saving }
                , E.attempt
                    CreatedNewPerson
                    (Person.createNewPerson personName)
                )

        CreatedNewPerson (Just personId) ->
            ( { model | newPerson = "", status = Created personId }
            , E.none
            )

        CreatedNewPerson Nothing ->
            ( { model | status = Failed "Acadia could not save that person." }
            , E.none
            )


document : Model -> Document Msg
document model =
    { title = "New person"
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
        [ H.div
            [ A.css
                [ S.bgGray1
                , S.col
                , S.g3
                , Css.maxWidth (Css.rem 42)
                , S.outdent
                , S.p3
                , S.wFull
                ]
            ]
            [ H.h1
                [ A.css
                    [ S.textGray3 ]
                ]
                [ H.text "New person" ]
            , H.div [ A.css [ S.col, S.g3 ] ]
                [ H.form
                    [ A.css
                        [ S.g3
                        , S.col
                        ]
                    , Event.onSubmit SubmittedPerson
                    ]
                    [ H.label [ A.css [ S.col, S.g2, S.textGray4 ] ]
                        [ H.span [] [ H.text "Person name" ]
                        , TextField.simple model.newPerson UpdatedNewPersonNameField
                            |> TextField.toHtml
                        ]
                    , H.div [ A.css [ S.row, S.justifyEnd ] ]
                        [ Button.primary
                            (if model.status == Saving then
                                "Saving..."

                             else
                                "Add person"
                            )
                            SubmittedPerson
                            |> Button.toHtml
                        ]
                    ]
                , statusView model.status
                ]
            ]
        ]


statusView : Status -> Html msg
statusView status =
    case status of
        Ready ->
            H.text ""

        Saving ->
            H.p [ A.css [ S.textGray4 ] ] [ H.text "Writing to Acadia..." ]

        Created personId ->
            H.p [ A.attribute "role" "status", A.css [ S.textGray4 ] ]
                [ H.text "Person created. "
                , H.a
                    [ Route.href (Route.Person personId)
                    , A.css
                        [ S.textGray4
                        , S.underline
                        , S.hover [ S.textGray5 ]
                        , Css.focus [ S.textGray5 ]
                        ]
                    ]
                    [ H.text "View person" ]
                ]

        Failed message ->
            H.p [ A.css [ S.textRed1 ] ] [ H.text message ]
