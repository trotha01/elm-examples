module Main exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Css exposing (backgroundColor, backgroundImage, property, height, width, px, hsl)
import Css.Colors exposing (blue)


main : Html a
main =
    clouds


clouds : Html a
clouds =
    div
        [ styles
            [ backgroundColor (hsl 34 53 82)
            , backgroundImage
                [ radialGradient ( Px 30, Px 40 )
                    Ellipse
                    Cover
                    [ ( white 0.5, "0px" )
                    , ( white 0.5, "20px" )
                    , ( transparent, "30px" )
                    ]
                , radialGradient ( Px 20, Px 50 )
                    Ellipse
                    Cover
                    [ ( white 1, "0px" )
                    , ( white 1, "20px" )
                    , ( darkBlue 0.5, "30px" )
                    ]
                ]
            , width (px 100)
            , height (px 100)
              -- , backgroundSize ( px 100, px 100 )
            ]
        ]
        []


backgroundSize : List String -> String
backgroundSize backgrounds =
    String.join "," backgrounds


desert : Html a
desert =
    div
        [ styles
            [ backgroundColor (hsl 34 53 82)
            , backgroundImage
                [ repeatingLinearGradient 45
                    [ ( brown 0.5, "0px" )
                    , ( brown 0.5, "2px" )
                    , ( yellow 0.5, "4px" )
                    , ( yellow 0.5, "6px" )
                    ]
                , repeatingLinearGradient -45
                    [ ( brown 0.5, "0px" )
                    , ( brown 0.5, "5px" )
                    , ( yellow 0.5, "7px" )
                    , ( yellow 0.5, "10px" )
                    ]
                ]
            , width (px 100)
            , height (px 100)
            ]
        ]
        []


transparent : String
transparent =
    -- "hsla(10, 100%, 100%, 0)"
    "transparent"


white : Float -> String
white alpha =
    "hsla(0, 100%, 100%, " ++ toString alpha ++ ")"


lightBlue : Float -> String
lightBlue alpha =
    "hsla(179, 100%, 75%, " ++ toString alpha ++ ")"


darkBlue : Float -> String
darkBlue alpha =
    "hsla(215, 100%, 43%, " ++ toString alpha ++ ")"


brown : Float -> String
brown alpha =
    "hsla(41, 62%, 50%, " ++ toString alpha ++ ")"


yellow : Float -> String
yellow alpha =
    "hsla(5, 53%, 63%, " ++ toString alpha ++ ")"



-- GRADIENT TYPE


type alias Gradient =
    ( String, String )


gradientToString : Gradient -> String
gradientToString ( color, stop ) =
    color ++ " " ++ stop


gradientsToString : List Gradient -> String
gradientsToString gradients =
    gradients
        |> List.map gradientToString
        |> String.join ","



-- HELPERS


backgroundImage : List String -> Css.Mixin
backgroundImage imgs =
    property "background-image" (String.join ", " imgs)


linearGradient : Int -> List Gradient -> String
linearGradient angle gradients =
    gradientProperty "linear-gradient" [ (toString angle) ++ "deg" ] gradients


repeatingLinearGradient : Int -> List Gradient -> String
repeatingLinearGradient angle gradients =
    gradientProperty "repeating-linear-gradient" [ (toString angle) ++ "deg" ] gradients


type Shape
    = Circle
    | Ellipse


type Size
    = ClosestSide
    | ClosestCorner
    | FarthestSide
    | FarthestCorner
    | Contain
    | Cover


type Unit
    = Px Int
    | Perc Int


type alias Position =
    ( Unit, Unit )


unitToString : Unit -> String
unitToString unit =
    case unit of
        Px u ->
            toString u ++ "px"

        Perc u ->
            toString u ++ "%"


positionToString : Position -> String
positionToString ( x, y ) =
    unitToString x ++ " " ++ unitToString y


radialGradient : Position -> Shape -> Size -> List Gradient -> String
radialGradient position shape size gradients =
    gradientProperty
        "-webkit-radial-gradient"
        [ positionToString position
        , (toString shape) ++ " " ++ (toString size)
        ]
        gradients


gradientProperty : String -> List String -> List Gradient -> String
gradientProperty gradient metadata gradients =
    (gradient
        ++ "("
        ++ String.join "," metadata
        ++ ", "
        ++ gradientsToString gradients
        ++ ")"
    )


styles =
    Css.asPairs >> Html.Attributes.style
