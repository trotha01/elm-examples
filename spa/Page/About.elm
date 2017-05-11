module Page.About exposing (view)

import Html exposing (Html, button, div, text, a)
import Route


view : Html msg
view =
    div []
        [ homeButton
        , about
        ]


about : Html msg
about =
    div []
        [ text "ABOUT"
        ]


homeButton : Html msg
homeButton =
    div []
        [ a [ Route.href Route.Home ] [ text "Home" ]
        ]
