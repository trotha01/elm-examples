module Page.Home exposing (view)

import Html exposing (Html, button, div, text, a)
import Route


view : Html msg
view =
    div []
        [ aboutButton
        , home
        ]


home : Html msg
home =
    div []
        [ text "home"
        ]


aboutButton : Html msg
aboutButton =
    div []
        [ a [ Route.href Route.About ] [ text "About" ]
        ]
