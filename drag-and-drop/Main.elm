module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Json.Decode as Decode
import Mouse exposing (Position)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { boxes : List Box }


type alias Box =
    { id : Int
    , position : Position
    , drag : Maybe Drag
    }


type alias Drag =
    { start : Position
    , current : Position
    }


init : ( Model, Cmd Msg )
init =
    ( { boxes = [ sampleBox 0 ] }, Cmd.none )


sampleBox id =
    Box id (Position 200 200) Nothing



-- UPDATE


type Msg
    = AddBox
    | DeleteBox Int
    | BoxMsg ( Int, BoxMsg )


type BoxMsg
    = DragStart Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddBox ->
            ( { model | boxes = sampleBox (List.length model.boxes) :: model.boxes }, Cmd.none )

        DeleteBox id ->
            ( { model | boxes = List.filter (\box -> box.id /= id) model.boxes }, Cmd.none )

        BoxMsg ( id, boxMsg ) ->
            let
                newBoxes =
                    List.map
                        (\box ->
                            if box.id == id then
                                updateBox boxMsg box
                            else
                                box
                        )
                        model.boxes
            in
            ( { model | boxes = newBoxes }, Cmd.none )


updateBox : BoxMsg -> Box -> Box
updateBox msg ({ id, position, drag } as box) =
    case msg of
        DragStart xy ->
            Box id position (Just (Drag xy xy))

        DragAt xy ->
            Box id position (Maybe.map (\{ start } -> Drag start xy) drag)

        DragEnd _ ->
            Box id (getPosition box) Nothing



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subs =
            List.map
                (\box ->
                    case box.drag of
                        Nothing ->
                            Sub.none

                        Just _ ->
                            Sub.batch
                                [ Mouse.moves (\pos -> BoxMsg ( box.id, DragAt pos ))
                                , Mouse.ups (\pos -> BoxMsg ( box.id, DragEnd pos ))
                                ]
                )
                model.boxes
    in
    Sub.batch subs



-- VIEW


(=>) =
    (,)


view : Model -> Html Msg
view model =
    div []
        (addBoxButton
            :: List.map viewBox model.boxes
        )


addBoxButton =
    button [ onClick AddBox ] [ text "Add Box" ]


viewBox : Box -> Html Msg
viewBox box =
    let
        realPosition =
            getPosition box
    in
    div
        [ onMouseDown box.id
        , style
            [ "background-color" => "#3C8D2F"
            , "cursor" => "move"
            , "width" => "100px"
            , "height" => "100px"
            , "border-radius" => "4px"
            , "position" => "absolute"
            , "left" => px realPosition.x
            , "top" => px realPosition.y
            , "color" => "white"
            , "display" => "flex"
            , "align-items" => "center"
            , "justify-content" => "center"
            ]
        ]
        [ text <| toString box.id ++ ": Drag Me! "
        , div
            [ -- on "click" (Decode.succeed (DeleteBox box.id))
              onClick (DeleteBox box.id)
            , style
                [ ( "cursor", "pointer" )
                , ( "color", "red" )
                , ( "float", "right" )
                , ( "", "" )
                ]
            ]
            [ text "X" ]
        ]


px : Int -> String
px number =
    toString number ++ "px"


getPosition : Box -> Position
getPosition { position, drag } =
    case drag of
        Nothing ->
            position

        Just { start, current } ->
            Position
                (position.x + current.x - start.x)
                (position.y + current.y - start.y)


onMouseDown : Int -> Attribute Msg
onMouseDown id =
    on "mousedown" (Decode.map (\pos -> BoxMsg ( id, DragStart pos )) Mouse.position)
