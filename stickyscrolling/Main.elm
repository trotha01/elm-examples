module Main exposing (main)

import Dom
import Dom.Scroll
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Task
import Time


main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { content : List String
    , isAtBottom : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { content = [], isAtBottom = False }, Cmd.none )



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (Time.second * 1) (always <| AddLine)



-- UPDATE


type Msg
    = AddLine
    | Scroll Bool
    | ScrollToBottom (Result Dom.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddLine ->
            if model.isAtBottom then
                ( { model | content = model.content ++ [ "content" ] }, scrollToBottom )
            else
                ( { model | content = model.content ++ [ "content" ] }, Cmd.none )

        Scroll atBottom ->
            ( { model | isAtBottom = atBottom }, Cmd.none )

        ScrollToBottom result ->
            case result of
                Ok _ ->
                    ( model, Cmd.none )

                Err e ->
                    Debug.log "err" e
                        |> always ( model, Cmd.none )


scrollToBottom : Cmd Msg
scrollToBottom =
    Dom.Scroll.toBottom "content"
        |> Task.attempt ScrollToBottom



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "height", "90px" )
            , ( "overflow", "scroll" )
            , ( "position", "sticky" )
            , ( "border", "1px solid black" )
            ]
        , id "content"
        , on "scroll" onScrollDecoder
        ]
        (List.indexedMap viewLine model.content)


viewLine : Int -> String -> Html Msg
viewLine index line =
    div [] [ text <| toString index ++ ": " ++ line ]


onScrollDecoder : Decode.Decoder Msg
onScrollDecoder =
    -- Decode.succeed (Scroll True)
    Decode.map3
        (\sh oh st -> Scroll (isScrollAtBottom sh oh st))
        (Decode.at [ "target", "scrollHeight" ] Decode.float)
        (Decode.at [ "target", "offsetHeight" ] Decode.float)
        (Decode.at [ "target", "scrollTop" ] Decode.float)


isScrollAtBottom : Float -> Float -> Float -> Bool
isScrollAtBottom scrollHeight offsetHeight scrollTop =
    scrollHeight - offsetHeight - scrollTop < 1
