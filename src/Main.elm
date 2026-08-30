module Main exposing (main)

import Acadia.Transaction
import Backend
import Browser
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Event
import Style
import View.Button as Button
import View.TextField as TextField


endpointUrl : String
endpointUrl =
    "/_endpoints"


type alias Model =
    { people : List String
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


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view >> Html.toUnstyled
        }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model : Model
        model =
            { people = []
            , newPerson = ""
            , status = Loading
            }
    in
    ( model
    , getPeople
    )


getPeople : Cmd Msg
getPeople =
    Acadia.Transaction.attempt endpointUrl GotPeople Backend.getPeople


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
                , Acadia.Transaction.attempt endpointUrl AddedPerson (Backend.createNewPerson person)
                )

        AddedPerson (Just ()) ->
            ( { model | newPerson = "", status = Loading }, getPeople )

        AddedPerson Nothing ->
            ( { model | status = Failed "Acadia could not save that person." }, Cmd.none )

        GotPeople (Just people) ->
            ( { model | people = people, status = Ready }, Cmd.none )

        GotPeople Nothing ->
            ( { model | status = Failed "Could not read people from Acadia." }, Cmd.none )


view : Model -> Html Msg
view model =
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
            [ Html.h1 [ Attr.css [ Style.fontBold, Style.mb4, Style.text4xl ] ] [ Html.text "People" ]
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
