module Person exposing
    ( Model
    , Msg
    , init
    , setShared
    , shared
    , update
    , view
    )

import Backend exposing (Person, PersonPageFlags)
import Document exposing (Document)
import Effect exposing (Eff)
import Html.Styled as H
import Html.Styled.Attributes as A
import PersonId.Util as PersonIdUtil
import Shared
import Style as S


type alias Model =
    { shared : Shared.Model
    , person : Person
    }


type Msg
    = NoMsgYet


init : Shared.Model -> PersonPageFlags -> Model
init sharedModel flags =
    { shared = sharedModel
    , person = flags.person
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
        NoMsgYet ->
            ( model, Effect.none )


view : Model -> Document msg
view model =
    { title = model.person.name
    , body = [ page model ]
    }


page : Model -> H.Html msg
page model =
    H.div
        [ A.css
            [ S.minHFullViewport
            , S.justifyCenter
            , S.p4
            , S.row
            , S.wFull
            ]
        ]
        [ H.article
            [ A.css
                [ S.bgGray1
                , S.maxW192
                , S.outdent
                , S.p2
                , S.wFull
                ]
            ]
            [ H.h1
                [ A.css
                    [ S.m0
                    , S.p2
                    , S.px3
                    , S.textGray3
                    ]
                ]
                [ H.text model.person.name ]
            , H.dl
                [ A.css
                    [ S.m0
                    , S.m2
                    , S.p3
                    ]
                ]
                [ detail "Name" model.person.name
                , detail "ID" (PersonIdUtil.toString model.person.id)
                ]
            ]
        ]


detail : String -> String -> H.Html msg
detail label value =
    H.div [ A.css [ S.g2, S.p2, S.row ] ]
        [ H.dt [ A.css [ S.textGray3 ] ] [ H.text label ]
        , H.dd [ A.css [ S.m0, S.textGray4 ] ] [ H.text value ]
        ]
