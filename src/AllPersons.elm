module AllPersons exposing
    ( Model
    , Msg
    , init
    , setShared
    , shared
    , update
    , view
    )

import Css
import Document exposing (Document)
import Effect as E exposing (Eff)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Person
import PersonId.Util as PersonIdUtil
import Route
import Shared
import Style as S
import View.Button as Button


type alias Model =
    { shared : Shared.Model
    , people : People
    }


type People
    = Loading
    | Loaded (List Person.Person)
    | Failed


type Msg
    = PeopleResponseReceived (Maybe (List Person.Person))
    | RetryButtonClicked


init : Shared.Model -> ( Model, Eff Msg )
init sharedModel =
    ( { shared = sharedModel, people = Loading }
    , loadPeople
    )


loadPeople : Eff Msg
loadPeople =
    E.attempt PeopleResponseReceived Person.getAllPersons


shared : Model -> Shared.Model
shared model =
    model.shared


setShared : Shared.Model -> Model -> Model
setShared sharedModel model =
    { model | shared = sharedModel }


update : Msg -> Model -> ( Model, Eff Msg )
update msg model =
    case msg of
        PeopleResponseReceived result ->
            ( { model
                | people =
                    case result of
                        Just people ->
                            Loaded (List.sortBy (String.toLower << .name) people)

                        Nothing ->
                            Failed
              }
            , E.none
            )

        RetryButtonClicked ->
            ( { model | people = Loading }, loadPeople )


view : Model -> Document Msg
view model =
    { title = "All persons"
    , body =
        [ H.div
            [ A.css [ S.minHFullViewport, Css.alignItems Css.flexStart, S.justifyCenter, S.p4, S.row, S.wFull ] ]
            [ H.article
                [ A.css [ S.bgGray1, S.col, S.g3, S.minW0, S.outdent, S.p3, S.wFull ] ]
                [ H.h1 [ A.css [ S.textGray3 ] ] [ H.text "All persons" ]
                , peopleView model.people
                ]
            ]
        ]
    }


peopleView : People -> Html Msg
peopleView people =
    case people of
        Loading ->
            H.p [ A.attribute "role" "status" ] [ H.text "Loading people…" ]

        Failed ->
            H.div [ A.css [ S.col, S.g2, Css.alignItems Css.flexStart ] ]
                [ H.p [ A.attribute "role" "alert" ] [ H.text "Could not load people. Please try again." ]
                , Button.secondary "Try again" RetryButtonClicked |> Button.toHtml
                ]

        Loaded [] ->
            H.div [ A.css [ S.col, S.g2 ] ]
                [ H.p [] [ H.text "No people yet." ]
                , H.a [ Route.href Route.NewPerson, A.css [ S.link ] ] [ H.text "Create a person" ]
                ]

        Loaded persons ->
            H.ul [ A.css [ S.col, S.g2, Css.property "list-style" "none" ] ]
                (List.map personLink persons)


personLink : Person.Person -> Html msg
personLink person =
    H.li []
        [ H.a
            [ Route.href (Route.Person person.id)
            , A.css
                [ S.link
                , Css.property "overflow-wrap" "anywhere"
                ]
            ]
            [ H.text
                (if String.isEmpty (String.trim person.name) then
                    "Person " ++ PersonIdUtil.toString person.id

                 else
                    person.name
                )
            ]
        ]
