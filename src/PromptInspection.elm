module PromptInspection exposing (view)

import Css
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Json.Decode as D
import Style as S


type alias Source =
    { source : String
    , reason : String
    , text : String
    }


type alias Snapshot =
    { included : List Source
    , omitted : List Source
    , budget : Int
    , instructions : Maybe String
    }


snapshotDecoder : D.Decoder Snapshot
snapshotDecoder =
    D.map4 Snapshot
        (D.at [ "selection", "included" ] (D.list sourceDecoder))
        (D.at [ "selection", "omitted" ] (D.list sourceDecoder))
        (D.at [ "selection", "context_budget" ] D.int)
        (D.maybe (D.at [ "request", "instructions" ] D.string))


sourceDecoder : D.Decoder Source
sourceDecoder =
    D.map3 Source
        (D.field "source" D.string)
        (D.field "reason" D.string)
        (D.field "text" D.string)


view : String -> Html msg
view raw =
    case D.decodeString snapshotDecoder raw of
        Err _ ->
            textBlock raw

        Ok snapshot ->
            H.div [ A.css [ S.col, S.g3, S.minW0 ] ]
                [ H.p [] [ H.text ("Context budget: " ++ String.fromInt snapshot.budget ++ " UTF-8 bytes. This is not a token count.") ]
                , case snapshot.instructions of
                    Nothing ->
                        H.text ""

                    Just instructions ->
                        disclosure "Application instructions" [ textBlock instructions ]
                , H.h3 [ A.css [ S.textGray3 ] ] [ H.text ("Included context (" ++ String.fromInt (List.length snapshot.included) ++ ")") ]
                , H.div [ A.css [ S.col, S.g2 ] ] (List.map sourceView snapshot.included)
                , disclosure ("Excluded context (" ++ String.fromInt (List.length snapshot.omitted) ++ ")")
                    (if List.isEmpty snapshot.omitted then
                        [ H.p [] [ H.text "No inputs were excluded." ] ]

                     else
                        List.map sourceView snapshot.omitted
                    )
                , disclosure "Exact saved request and selection" [ textBlock raw ]
                ]


sourceView : Source -> Html msg
sourceView source =
    H.div [ A.css [ S.col, S.g2, Css.property "overflow-wrap" "anywhere" ] ]
        [ H.p [ A.css [ S.textGray3 ] ] [ H.text (source.source ++ " · " ++ source.reason) ]
        , textBlock source.text
        ]


disclosure : String -> List (Html msg) -> Html msg
disclosure label children =
    H.details [ A.css [ S.col, S.g2, S.minW0 ] ]
        [ H.summary [] [ H.text label ]
        , H.div [ A.css [ S.col, S.g3, S.minW0 ] ] children
        ]


textBlock : String -> Html msg
textBlock text =
    H.pre [ A.css [ Css.whiteSpace Css.preWrap, Css.property "overflow-wrap" "anywhere", S.minW0 ] ] [ H.text text ]
