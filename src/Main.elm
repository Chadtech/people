module Main exposing (main)

import Acadia.Transaction
import Backend
import Browser
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attr
import Html.Styled.Events as Event
import Style


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
                [ Html.input
                    [ Attr.value model.newPerson
                    , Event.onInput ChangedNewPerson
                    , Attr.placeholder "Add a person"
                    , Attr.disabled (model.status == Saving)
                    , Attr.css
                        [ Style.bgNightwood1
                        , Style.border
                        , Style.borderGray2
                        , Style.flex1
                        , Style.minW0
                        , Style.outlineNone
                        , Style.px3
                        , Style.py2
                        , Style.rounded
                        , Style.textGray5
                        ]
                    ]
                    []
                , Html.button
                    [ Attr.disabled (String.isEmpty (String.trim model.newPerson) || model.status == Saving)
                    , Attr.css
                        [ Style.bgYellow1
                        , Style.borderNone
                        , Style.cursorPointer
                        , Style.hover [ Style.bgGray1 ]
                        , Style.px4
                        , Style.py2
                        , Style.rounded
                        , Style.textYellow5
                        ]
                    ]
                    [ Html.text
                        (if model.status == Saving then
                            "Saving..."

                         else
                            "Add"
                        )
                    ]
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
