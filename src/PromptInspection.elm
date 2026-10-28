module PromptInspection exposing (view)

import Acadia.UInt64 as UInt64
import CognitiveTrace exposing (CognitiveTrace, Stage(..))
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Snapshot exposing (Snapshot, Source, Sources(..))
import Style as S


view : Snapshot -> Html msg
view snapshot =
    let
        included : List Source
        included =
            sourcesToList snapshot.included

        omitted : List Source
        omitted =
            sourcesToList snapshot.omitted

        instructionsView : Html msg
        instructionsView =
            case snapshot.instructions of
                Nothing ->
                    H.text ""

                Just instructions ->
                    disclosure "Application instructions" [ textBlock instructions ]

        stageDescription : String
        stageDescription =
            case snapshot.cognition of
                Just trace ->
                    case trace.stage of
                        Observing ->
                            "Appraisal stage. Included context below belongs to the outside observer."

                        Speaking ->
                            "Narration stage. Included context below is what the speaking person can access. Private evidence is available only in inspection."

                Nothing ->
                    "Saved generation context."

        cognitiveViews : List (Html msg)
        cognitiveViews =
            case snapshot.cognition of
                Just trace ->
                    traceViews trace

                Nothing ->
                    []

        omittedViews : List (Html msg)
        omittedViews =
            if List.isEmpty omitted then
                [ H.p [] [ H.text "No inputs were excluded." ] ]

            else
                List.map sourceView omitted
    in
    H.div [ A.css [ S.col, S.g3, S.minW0 ] ]
        [ H.p [] [ H.text stageDescription ]
        , H.div [ A.css [ S.col, S.g2 ] ] cognitiveViews
        , H.p [] [ H.text ("Context budget: " ++ UInt64.toString snapshot.budget ++ " UTF-8 bytes. This is not a token count.") ]
        , instructionsView
        , H.h3 [ A.css [ S.textGray3 ] ] [ H.text ("Included context (" ++ String.fromInt (List.length included) ++ ")") ]
        , H.div [ A.css [ S.col, S.g2 ] ] (List.map sourceView included)
        , disclosure ("Excluded context (" ++ String.fromInt (List.length omitted) ++ ")") omittedViews
        , disclosure "Exact saved requests, transcript, and selection" [ textBlock snapshot.raw ]
        ]


sourcesToList : Sources -> List Source
sourcesToList sources =
    case sources of
        NoSources ->
            []

        SourceItem source rest ->
            source :: sourcesToList rest


sourceView : Source -> Html msg
sourceView source =
    H.div [ A.css [ S.col, S.g2, S.wrapAnywhere ] ]
        [ H.p [ A.css [ S.textGray3 ] ] [ H.text (source.source ++ " · " ++ source.selectionReason) ]
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
    H.pre [ A.css [ S.whitespacePreWrap, S.wrapAnywhere, S.minW0 ] ] [ H.text text ]


traceViews : CognitiveTrace -> List (Html msg)
traceViews trace =
    let
        optionalDisclosure : ( String, Maybe String ) -> Maybe (Html msg)
        optionalDisclosure ( label, value ) =
            Maybe.map (\text -> disclosure label [ textBlock text ]) value
    in
    [ disclosure "Private dispositions before selection" [ textBlock trace.dispositions ] ]
        ++ List.filterMap optionalDisclosure
            [ ( "Third-person predictions and uncertainty", trace.predictions )
            , ( "Selected action and accessible feeling", trace.inclination )
            ]
        ++ [ disclosure "Exact appraisal request and response" [ textBlock trace.appraisal ] ]
