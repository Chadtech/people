module NewPerson exposing
    ( Model
    , Msg
    , document
    , init
    , setShared
    , shared
    , update
    )

import Api
import Backend
import Css
import Document exposing (Document)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as Event
import PersonId exposing (PersonId)
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdatedNewPersonNameField newPerson ->
            ( { model | newPerson = newPerson }, Cmd.none )

        SubmittedPerson ->
            let
                person =
                    String.trim model.newPerson
            in
            if String.isEmpty person || model.status == Saving then
                ( model, Cmd.none )

            else
                ( { model | status = Saving }
                , Api.attempt CreatedNewPerson (Backend.createNewPerson person)
                )

        CreatedNewPerson (Just personId) ->
            ( { model | newPerson = "", status = Ready }
            , Cmd.none
            )

        CreatedNewPerson Nothing ->
            ( { model | status = Failed "Acadia could not save that person." }
            , Cmd.none
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
            [ Css.minHeight (Css.vh 100)
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
                , Css.maxWidth (Css.rem 42)
                , S.outdent
                , S.p2
                , S.wFull
                ]
            ]
            [ H.h1
                [ A.css
                    [ S.fontBold
                    , S.m0
                    , S.p2
                    , S.px3
                    , S.textGray3
                    ]
                ]
                [ H.text "New person" ]
            , H.div [ A.css [ S.p3 ] ]
                [ H.form
                    [ A.css
                        [ S.g3
                        , S.col
                        ]
                    , Event.onSubmit SubmittedPerson
                    ]
                    [ H.label [ A.css [ S.col, S.g2, S.textGray4 ] ]
                        [ H.span [ A.css [ S.fontBold ] ] [ H.text "Person name" ]
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

        Failed message ->
            H.p [ A.css [ S.textRed1 ] ] [ H.text message ]
