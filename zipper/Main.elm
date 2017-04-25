module Main exposing (..)

import Html exposing (Html, text, div, button)
import Html.Events exposing (onClick)


main =
    Html.beginnerProgram { model = init, view = view, update = update }



-- MODEL


type alias Model =
    { selection : Int
    , options : List String
    }


init : Model
init =
    { selection = 0
    , options = [ "one", "two", "three" ]
    }



-- UPDATE


type Msg
    = Next
    | Previous


update : Msg -> Model -> Model
update msg model =
    case msg of
        Next ->
            { model | selection = nextOption model.selection model.options }

        Previous ->
            { model | selection = previousOption model.selection model.options }


nextOption : Int -> List String -> Int
nextOption n options =
    (n + 1)
        |> min ((List.length options) - 1)


previousOption : Int -> List String -> Int
previousOption n options =
    (n - 1)
        |> max 0


currentOption : Int -> List String -> String
currentOption n options =
    options
        |> List.drop n
        |> List.head
        |> Maybe.withDefault ""



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Previous ] [ text "<-" ]
        , button [ onClick Next ] [ text "->" ]
        , div []
            [ text (currentOption model.selection model.options)
            ]
        ]
