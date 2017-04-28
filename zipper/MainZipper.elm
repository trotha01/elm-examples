module Main exposing (..)

import Html exposing (Html, text, div, button)
import Html.Events exposing (onClick)
import List.Zipper as Zipper exposing (Zipper(..))


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
            { model | options = nextOption model.options }

        Previous ->
            { model | options = previousOption model.options }


nextOption : Zipper String -> Zipper String
nextOption options =
    options
        |> Zipper.next
        |> Maybe.withDefault options


previousOption : Zipper String -> Zipper String
previousOption options =
    options
        |> Zipper.previous
        |> Maybe.withDefault options


currentOption : Zipper String -> String
currentOption options =
    options
        |> Zipper.current



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Previous ] [ text "<-" ]
        , button [ onClick Next ] [ text "->" ]
        , div []
            [ text (currentOption model.options)
            ]
        ]
