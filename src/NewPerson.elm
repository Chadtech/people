module NewPerson exposing
    ( Model
    , Msg
    , init
    , shared
    , update
    , view
    )

import Api
import Backend
import Browser
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Event
import Shared
import Style
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
    = ChangedNewPerson String
    | SubmittedPerson
    | AddedPerson (Maybe ())
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


getPeople : Cmd Msg
getPeople =
    Api.attempt GotPeople Backend.getPeople


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedNewPerson newPerson ->
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
                , Api.attempt AddedPerson (Backend.createNewPerson person)
                )

        AddedPerson (Just ()) ->
            ( { model | newPerson = "", status = Loading }
            , getPeople
            )

        AddedPerson Nothing ->
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


view : Model -> Browser.Document Msg
view model =
    { title = "New person"
    , body = [ page model |> Html.toUnstyled ]
    }


page : Model -> Html Msg
page model =
    Html.div
        [ Attr.css
            [ Style.bgNightwood0
            , Style.hFullViewport
            , Style.justifyCenter
            , Style.row
            , Style.textGray5
            , Style.wFull
            ]
        ]
        [ Html.div
            [ Attr.css
                [ Style.maxW32
                , Style.px4
                , Style.py16
                , Style.wFull
                ]
            ]
            [ Html.h1 [ Attr.css [ Style.fontBold, Style.mb4, Style.text4xl ] ] [ Html.text "New person" ]
            , Html.form [ Attr.css [ Style.g2, Style.row ], Event.onSubmit SubmittedPerson ]
                [ Html.div [ Attr.css [ Style.flex1, Style.minW0 ] ]
                    [ TextField.simple model.newPerson ChangedNewPerson
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
            , Html.ul [ Attr.css [ Style.p0 ] ]
                (List.map
                    (\person -> Html.li [ Attr.css [ Style.py1 ] ] [ Html.text person ])
                    model.people
                )
            ]
        ]


statusView : Status -> Html.Html msg
statusView status =
    case status of
        Loading ->
            Html.p [] [ Html.text "Loading from Acadia..." ]

        Ready ->
            Html.text ""

        Saving ->
            Html.p [] [ Html.text "Writing to Acadia..." ]

        Failed message ->
            Html.p [ Attr.css [ Style.textRed1 ] ] [ Html.text message ]
