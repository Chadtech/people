module NewPerson exposing
    ( Model
    , Msg
    , init
    , setShared
    , shared
    , update
    , view
    )

import Api
import Backend
import Css
import Document exposing (Document)
import Html.Styled as H
import Html.Styled.Attributes as A
import Html.Styled.Events as Event
import PersonId exposing (PersonId)
import Shared
import Style as S
import View.Button as Button
import View.TextField as TextField


type alias Model =
    { shared : Shared.Model
    , people : List String
    , newPerson : String
    , status : Status
    }


type Status
    = Loading
    | Ready
    | Saving
    | Failed String


type Msg
    = UpdatedNewPersonNameField String
    | SubmittedPerson
    | CreatedNewPerson (Maybe PersonId)
    | GotPeople (Maybe (List String))


init : Shared.Model -> ( Model, Cmd Msg )
init sharedModel =
    ( { shared = sharedModel
      , people = []
      , newPerson = ""
      , status = Loading
      }
    , getPeople
    )


shared : Model -> Shared.Model
shared model =
    model.shared


setShared : Shared.Model -> Model -> Model
setShared sharedModel model =
    { model | shared = sharedModel }


getPeople : Cmd Msg
getPeople =
    Api.attempt GotPeople Backend.getPeople


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
            ( { model | newPerson = "", status = Loading }
            , getPeople
            )

        CreatedNewPerson Nothing ->
            ( { model | status = Failed "Acadia could not save that person." }
            , Cmd.none
            )

        GotPeople (Just people) ->
            ( { model | people = people, status = Ready }
            , Cmd.none
            )

        GotPeople Nothing ->
            ( { model | status = Failed "Could not read people from Acadia." }
            , Cmd.none
            )


view : Model -> Document Msg
view model =
    { title = "New person"
    , body = [ page model ]
    }


page : Model -> H.Html Msg
page model =
    H.div
        [ A.css
            [ Css.minHeight (Css.vh 100)
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
                    [ S.bgNightwood1
                    , S.fontBold
                    , S.indent
                    , S.m0
                    , S.p2
                    , S.px3
                    , S.textLg
                    , S.textGray4
                    ]
                ]
                [ H.text "New person" ]
            , H.div [ A.css [ S.p3 ] ]
                [ H.form
                    [ A.css
                        [ S.bgNightwood1
                        , S.g2
                        , S.indent
                        , S.p3
                        , S.row
                        ]
                    , Event.onSubmit SubmittedPerson
                    ]
                    [ H.div [ A.css [ S.flex1, S.minW0 ] ]
                        [ TextField.simple model.newPerson UpdatedNewPersonNameField
                            |> TextField.toHtml
                        ]
                    , Button.primary
                        (if model.status == Saving then
                            "Saving..."

                         else
                            "Add"
                        )
                        SubmittedPerson
                        |> Button.toHtml
                    ]
                , statusView model.status
                , H.ul
                    [ A.css
                        [ S.bgNightwood1
                        , S.indent
                        , S.m0
                        , S.p2
                        , Css.property "list-style" "none"
                        , Css.property "min-height" "7rem"
                        ]
                    ]
                    (List.map personRow model.people)
                ]
            ]
        ]


personRow : String -> H.Html msg
personRow person =
    H.li
        [ A.css
            [ S.p2
            , S.px3
            , S.textGray4
            , Css.pseudoClass "nth-child(even)" [ S.bgNightwood2 ]
            ]
        ]
        [ H.text person ]


statusView : Status -> H.Html msg
statusView status =
    case status of
        Loading ->
            H.p [ A.css [ S.textGray3, S.textSm ] ] [ H.text "Loading from Acadia..." ]

        Ready ->
            H.text ""

        Saving ->
            H.p [ A.css [ S.textGray3, S.textSm ] ] [ H.text "Writing to Acadia..." ]

        Failed message ->
            H.p [ A.css [ S.textRed1 ] ] [ H.text message ]
