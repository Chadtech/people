module Sidebar exposing (Msg(..), view)

import Css
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A
import Html.Styled.Events as Ev
import Route
import Shared
import Style as S


type Msg
    = OpenToggleClicked


view : Shared.Model -> Html Msg
view shared =
    if shared.sidebarOpen then
        frame S.w64
            [ H.span [ A.css [ S.textGray3 ] ] [ H.text "People" ]
            , toggle "Close navigation" "←"
            ]
            [ navigation ]

    else
        frame S.w16
            [ toggle "Open navigation" "☰" ]
            []


frame : Css.Style -> List (Html Msg) -> List (Html Msg) -> Html Msg
frame width header content =
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
            , width
            ]
        ]
        (H.header
            [ A.css
                [ S.itemsCenter
                , S.justifySpaceBetween
                , S.m2
                , S.p2
                , S.row
                , S.textGray4
                ]
            ]
            header
            :: content
        )


toggle : String -> String -> Html Msg
toggle label glyph =
    H.button
        [ A.attribute "aria-label" label
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
        , Ev.onClick OpenToggleClicked
        ]
        [ H.text glyph ]


navigation : Html Msg
navigation =
    H.nav
        [ A.attribute "aria-label" "Main navigation"
        , A.css [ S.p2, S.pt1 ]
        ]
        [ H.ul
            [ A.css
                [ S.m0
                , S.p0
                , Css.property "list-style" "none"
                ]
            ]
            [ navItem Route.Conversations "Conversations"
            , navItem Route.AllPersons "All persons"
            , navItem Route.NewPerson "New person"
            ]
        ]


navItem : Route.Route -> String -> Html Msg
navItem route label =
    H.li []
        [ H.a
            [ Route.href route
            , A.css [ S.block, S.p2, S.px3, S.link ]
            ]
            [ H.text label ]
        ]
