module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder, andThen, decodeValue)


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



-- UPDATE


type Msg
    = Scroll Decode.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Scroll yOffset ->
            Debug.log "scrolled" yOffset
                |> always ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ header
        , parallax "buildings.jpeg"
        , body
        , parallaxVid "timelapse.mp4"
        , body
        ]


header : Html Msg
header =
    h3 [] [ text "Title" ]


body : Html Msg
body =
    div
        [ style
            [ ( "height", "500px" )
            , ( "background-color", "black" )
            , ( "color", "white" )
            , ( "font-size", "36px" )
            ]
        ]
        [ text "Some text here" ]


parallax : String -> Html Msg
parallax image =
    div
        [ style
            [ ( "background-image", """url(" """ ++ image ++ """ ")""" )
            , ( "min-height", "500px" )
            , ( "background-attachment", "fixed" )
            , ( "background-position", "center" )
            , ( "background-repeat", "no-repeat" )
            , ( "background-size", "cover" )
            ]
        ]
        []


parallaxVid : String -> Html Msg
parallaxVid video =
    node "video"
        [ style
            [ ( "width", "100%" )
            , ( "min-height", "300px" )
            ]
        , autoplay True
        , loop True
        ]
        [ source
            [ src video
            , style
                [ ( "type", "video/mp4" )
                ]
            ]
            []
        , text "Your browser does not support the video tag"
        ]



{-
   width="320" height="240" controls>
     <source src="movie.mp4" type="video/mp4">
       <source src="movie.ogg" type="video/ogg">
         Your browser does not support the video tag.
         </video>

     try : Decoder a -> Decoder a
     try decoder =
         Decode.value
             |> andThen
                 (\v ->
                     case Decode.decodeValue decoder v of
                         Ok _ ->
                             Debug.log "ok" e |> always decoder

                         Err e ->
                             Debug.log "error" e |> always decoder
                 )


     printKeys : Decoder a -> Decoder a
     printKeys decoder =
         Decode.value
             |> andThen
                 (\v ->
                     case Decode.decodeValue (Decode.keyValuePairs Decode.value) v of
                         Ok kvs ->
                             Debug.log "ok" (List.map Tuple.first kvs) |> always decoder

                         Err e ->
                             Debug.log "error" e |> always decoder
                 )


     onScrollDecoder : Decode.Decoder Msg
     onScrollDecoder =
         Decode.map Scroll (Decode.at [ "target" ] (printKeys Decode.value))
-}
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
