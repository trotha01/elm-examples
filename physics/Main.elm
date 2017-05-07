module Main exposing (..)

import AnimationFrame
import Html exposing (Html, div, text, button)
import Html.Attributes exposing (style, id)
import Html.Events exposing (onClick)
import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY)
import Time exposing (Time)
import Task
import Window
import Collision exposing (Velocity, Shape(Rect, Circle), Position, Dimensions, height, width, collideWith, collideList)


-- MAIN


main =
    Html.program { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


{-| The only reason walls are separate
 -  from objects is because of window resize,
 -  where we need to change the walls
-}
type alias Model =
    { objects : List Object
    , walls : List Object
    , window : Window.Size
    , pause : Bool
    }


type alias Object =
    { position : Position
    , velocity : Velocity
    , inverseMass : Float
    , restitution : Float
    , shape : Shape
    , id : String
    , color : String
    }


init : ( Model, Cmd Msg )
init =
    ( { pause = False
      , window = initWindow
      , objects =
            [ initCircle ( 100, 100 ) "red"
            , initCircle ( 110, 200 ) "orange"
            , initCircle ( 120, 300 ) "yellow"
            , initCircle ( 130, 400 ) "green"
            , initCircle ( 140, 500 ) "cyan"
            , initCircle ( 150, 600 ) "purple"
            ]
      , walls = []
      }
    , Task.perform WindowResize Window.size
    )


initWindow : Window.Size
initWindow =
    { width = 300, height = 300 }


{-| position is the center of the circle
-}
initCircle : ( Float, Float ) -> String -> Object
initCircle ( left, top ) color =
    { shape = Circle 32
    , position = vec2 left top
    , velocity = vec2 0.4 0.4
    , inverseMass = 0.5
    , restitution = 1
    , id = "ball"
    , color = color
    }


{-| position is the center of the rectangle
-}
initRect : Position -> Dimensions -> String -> Object
initRect position dimensions id =
    { shape = Rect dimensions
    , position = position
    , velocity = vec2 0 0
    , inverseMass = 0.01
    , restitution = 1
    , id = id
    , color = "blue"
    }


leftWall : ( Float, Float ) -> Object
leftWall ( width, height ) =
    initRect (vec2 1 (height / 2)) (vec2 1 height) "left"


rightWall : ( Float, Float ) -> Object
rightWall ( width, height ) =
    initRect (vec2 width (height / 2)) (vec2 1 height) "right"


topWall : ( Float, Float ) -> Object
topWall ( width, height ) =
    initRect (vec2 (width / 2) 1) (vec2 width 1) "top"


bottomWall : ( Float, Float ) -> Object
bottomWall ( width, height ) =
    initRect (vec2 (width / 2) height) (vec2 width 1) "bottom"



-- UPDATE


type Msg
    = Tick Time
    | WindowResize Window.Size
    | Pause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pause ->
            ( { model | pause = not model.pause }, Cmd.none )

        WindowResize size ->
            let
                map =
                    (( toFloat size.width, toFloat size.height ))
            in
                ( { model
                    | window = size
                    , objects =
                        model.objects
                    , walls = [ leftWall map, rightWall map, topWall map, bottomWall map ]
                  }
                , Cmd.none
                )

        Tick tDelta ->
            ( { model
                | objects =
                    model.objects
                        |> List.map (move tDelta)
                        |> collideList
                        |> List.map (collideWith model.walls)
                        |> List.map Tuple.first
              }
            , Cmd.none
            )


type alias Movable a =
    { a | position : Position, velocity : Velocity }


{-| move uses the time delta, position and velocity to calculate the new position
 - p1 = p0 + v0*dt
-}
move : Time -> Movable a -> Movable a
move tDelta ({ position, velocity } as object) =
    { object | position = Vec2.add position (Vec2.scale tDelta velocity) }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        (pause model.pause
            :: (List.map viewObject (model.objects ++ model.walls))
        )


pause : Bool -> Html Msg
pause paused =
    if paused then
        button [ onClick Pause ] [ text "Play" ]
    else
        button [ onClick Pause ] [ text "Pause" ]


viewObject : Object -> Html Msg
viewObject object =
    case object.shape of
        Circle radius ->
            viewCircle radius object

        Rect dimensions ->
            viewRect dimensions object


viewCircle : Float -> Object -> Html Msg
viewCircle radius object =
    div
        ([ style
            [ ( "border-radius", "50%" )
            , ( "background-color", object.color )
            , ( "border", "1px solid black" )
            , ( "position", "absolute" )
            , ( "left", (Vec2.getX object.position |> flip (-) (radius) |> toString |> flip (++) "px") )
            , ( "top", (Vec2.getY object.position |> flip (-) (radius) |> toString |> flip (++) "px") )
            , ( "width", (toString (radius * 2)) ++ "px" )
            , ( "height", (toString (radius * 2)) ++ "px" )
            ]
         ]
        )
        []


viewRect : Dimensions -> Object -> Html Msg
viewRect dimensions object =
    div
        ([ style
            [ ( "background-color", object.color )
            , ( "border", "1px solid black" )
            , ( "position", "absolute" )
            , ( "left", (Vec2.getX object.position |> flip (-) ((width dimensions) / 2) |> toString |> flip (++) "px") )
            , ( "top", (Vec2.getY object.position |> flip (-) ((height dimensions) / 2) |> toString |> flip (++) "px") )
            , ( "width", (toString <| width dimensions) ++ "px" )
            , ( "height", (toString <| height dimensions) ++ "px" )
            ]
         , id object.id
         ]
        )
        []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.pause then
        Sub.none
    else
        Sub.batch
            [ AnimationFrame.diffs Tick
            , Window.resizes WindowResize
            ]
