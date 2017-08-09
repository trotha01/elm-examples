module Page.About exposing (view)

import Html exposing (Html, a, button, div, text)
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
