module Sidebar exposing (Msg(..), view)

import Css
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as Event
import Route
import Shared
import Style as S


type Msg
    = OpenToggleClicked


view : Shared.Model -> Html Msg
view shared =
    H.aside
        [ A.css
            [ S.bgGray1
            , S.col
            , S.hFullViewport
            , S.outdent
            , Css.borderTopWidth (Css.px 0)
            , Css.borderBottomWidth (Css.px 0)
            , Css.borderLeftWidth (Css.px 0)
            , S.shrink0
            , if shared.sidebarOpen then
                S.w64

              else
                S.w16
            ]
        ]
        [ H.header
            [ A.css
                [ S.itemsCenter
                , S.justifySpaceBetween
                , S.m2
                , S.p2
                , S.row
                , S.textGray4
                ]
            ]
            [ if shared.sidebarOpen then
                H.span [ A.css [ S.textGray3 ] ] [ H.text "People" ]

              else
                H.text ""
            , H.button
                [ A.attribute "aria-label"
                    (if shared.sidebarOpen then
                        "Close navigation"

                     else
                        "Open navigation"
                    )
                , A.css
                    [ S.bgGray1
                    , S.cursorPointer
                    , S.outdent
                    , S.w8
                    , S.h8
                    , S.shrink0
                    , S.p0
                    , S.row
                    , S.itemsCenter
                    , Css.justifyContent Css.center
                    , S.textGray4
                    , Css.active [ S.indent ]
                    , Css.focus [ S.importantOutdent, S.outlineNone ]
                    ]
                , Event.onClick OpenToggleClicked
                ]
                [ H.text
                    (if shared.sidebarOpen then
                        "←"

                     else
                        "☰"
                    )
                ]
            ]
        , H.nav [ A.attribute "aria-label" "Main navigation", A.css [ S.p2, S.pt1 ] ]
            [ H.ul
                [ A.css
                    [ S.m0
                    , S.p0
                    , Css.property "list-style" "none"
                    ]
                ]
                [ H.li []
                    [ H.a [ Route.href Route.Conversations, A.title "Conversations", A.css [ S.block, S.p2, S.px3, S.link ] ]
                        [ H.text
                            (if shared.sidebarOpen then
                                "◇    Conversations"

                             else
                                "◇"
                            )
                        ]
                    ]
                , H.li []
                    [ H.a
                        [ Route.href Route.AllPersons
                        , A.title "All persons"
                        , A.css
                            [ S.block
                            , S.p2
                            , S.px3
                            , S.link
                            ]
                        ]
                        [ H.span [] [ H.text "≡" ]
                        , if shared.sidebarOpen then
                            H.span [ A.css [ S.ml4 ] ] [ H.text "All persons" ]

                          else
                            H.text ""
                        ]
                    ]
                , H.li []
                    [ H.a
                        [ Route.href Route.NewPerson
                        , A.title "New person"
                        , A.css
                            [ S.block
                            , S.p2
                            , S.px3
                            , S.link
                            ]
                        ]
                        [ H.span [] [ H.text "+" ]
                        , if shared.sidebarOpen then
                            H.span [ A.css [ S.ml4 ] ] [ H.text "New person" ]

                          else
                            H.text ""
                        ]
                    ]
                ]
            ]
        ]
