module Main exposing (..)

import Html exposing (Html, text, div, button)
import Html.Events exposing (onClick)
import Zipper exposing (Zipper)


main =
    Html.beginnerProgram { model = init, view = view, update = update }



-- MODEL


type alias Model =
    { options : Zipper String
    }


init : Model
init =
    { options = Zipper [] "one" [ "two", "three" ]
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


nextOption : Zipper String -> Zipper String
nextOption options =
    options
        |> Zipper.next
        |> Maybe.withDefault options


previousOption : Zipper String -> Zipper String
previousOption n options =
    options
        |> Zipper.previous
        |> Maybe.withDefault options


currentOption : Zipper String -> String
currentOption n options =
    options
        |> Zipper.current



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
