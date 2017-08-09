module Main exposing (..)

import Html exposing (Html, div, input, text)
import Html.Attributes exposing (value)
import Html.Events exposing (on)
import Json.Decode as Json


main =
    Html.beginnerProgram { model = init, update = update, view = view }



-- MODEL


type alias Model =
    { selection : Selection
    }


type alias Selection =
    { start : Int
    , end : Int
    }


initSelection : Selection
initSelection =
    { start = 0
    , end = 0
    }


init : Model
init =
    { selection = initSelection }



-- UPDATE


type Msg
    = Select Selection


update : Msg -> Model -> Model
update msg model =
    case msg of
        Select selection ->
            { model | selection = selection }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ value "Hello, World!", on "select" selectionDecoder ]
            []
        , div
            []
            [ text ("Start: " ++ toString model.selection.start) ]
        , div
            []
            [ text ("End: " ++ toString model.selection.end) ]
        ]


selectionDecoder : Json.Decoder Msg
selectionDecoder =
    Json.map Select <|
        Json.map2 Selection
            (Json.at [ "target", "selectionStart" ] Json.int)
            (Json.at [ "target", "selectionEnd" ] Json.int)
