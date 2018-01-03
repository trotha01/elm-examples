module Main exposing (..)

{-| An example of a sortable list using drag and drop
See the README.md file for more information
-}

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



{-
   TODO:
   Store all items in x-axis (1d)
   Update view to show items in 2d

-}
-- MODEL


type alias Model =
    { data : List String
    , drag : Maybe Drag
    }


init : ( Model, Cmd Msg )
init =
    { data = initialList |> List.sort
    , drag = Nothing
    }
        ! []


type alias Drag =
    { itemIndex : Int
    , startY : Int
    , currentY : Int
    , startX : Int
    , currentX : Int
    }


initialList : List String
initialList =
    [ "Shawshank Redemption"
    , "Godfather"
    , "Dark Knight"
    , "12 Angry Men"
    , "Schindler’s List"
    , "Pulp Fiction"
    , "Lord of the Rings"
    , "The Good, the Bad and the Ugly"
    , "Fight Club"
    , "The Empire Strikes Back"
    ]



-- UPDATE


type Msg
    = DragStart Int Position
    | DragAt Position
    | DragEnd Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragStart idx pos ->
            let
                newDrag =
                    { itemIndex = idx
                    , startX = pos.x
                    , currentX = pos.x
                    , startY = pos.y
                    , currentY = pos.y
                    }
            in
            { model | drag = Just newDrag } ! []

        DragAt pos ->
            { model
                | drag =
                    Maybe.map
                        (\{ itemIndex, startY, startX } ->
                            { itemIndex = itemIndex
                            , startX = startX
                            , currentX = pos.x
                            , startY = startY
                            , currentY = pos.y
                            }
                        )
                        model.drag
            }
                ! []

        DragEnd _ ->
            case model.drag of
                Just { itemIndex, startX, currentX, startY, currentY } ->
                    let
                        offsetY =
                            offset currentY startY

                        offsetX =
                            offset currentX startX
                    in
                    { model
                        | data = moveItem itemIndex offsetX offsetY model.data
                        , drag = Nothing
                    }
                        ! []

                Nothing ->
                    { model | drag = Nothing } ! []


offset : Int -> Int -> Int
offset current start =
    (current
        - start
        + (if current < start then
            -20
           else
            20
          )
    )
        // itemTotalHeight


{-| moveItem is called at the end of dragging to
change the order of the list
-}
moveItem : Int -> Int -> Int -> List a -> List a
moveItem fromPos offsetX offsetY list =
    let
        -- Remove the dragged object from the list
        listWithoutMoved =
            List.take fromPos list ++ List.drop (fromPos + 1) list

        -- select only the dragged object
        moved =
            List.take 1 <| List.drop fromPos list
    in
    List.take (fromPos + offsetY) listWithoutMoved
        ++ moved
        ++ List.drop (fromPos + offsetY) listWithoutMoved



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.drag of
        Nothing ->
            Sub.none

        Just _ ->
            Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style pageContainer ]
        [ div
            [ style listHeader ]
            [ h3
                [ style headerTitle ]
                [ text "Sortable favorite movies" ]
            ]

        -- , ul
        , div
            [ style listContainer ]
          <|
            List.indexedMap (itemView model) model.data
        ]


itemView : Model -> Int -> String -> Html Msg
itemView model idx item =
    let
        moveStyle =
            case model.drag of
                Just { itemIndex, startX, currentX, startY, currentY } ->
                    if itemIndex == idx then
                        [ ( "transform"
                          , ("translateY( " ++ toString (currentY - startY) ++ "px)")
                                ++ ("translateX( " ++ toString (currentX - startX) ++ "px)")
                                ++ "translateZ(10px)"
                          )
                        , ( "box-shadow", "0 3px 6px rgba(0,0,0,0.24)" )
                        , ( "willChange", "transform" )
                        ]
                    else
                        []

                Nothing ->
                    []

        makingWayStyle =
            case model.drag of
                Just { itemIndex, startY, currentY } ->
                    -- If the current item index (itemIndex) is greater than the item we're dragging (idx)
                    -- and if the amount the dragged item has moved so far is less than
                    --    the number of items between the dragged item and current item * size of each item
                    if (idx < itemIndex) && (currentY - startY) < (idx - itemIndex) * itemTotalHeight + 20 then
                        [ -- ( "transform", "translateY(50px)" )
                          ( "transform", translateY itemTotalHeight )
                        , ( "transition", "transform 200ms ease-in-out" )
                        ]
                    else if (idx > itemIndex) && (currentY - startY) > (idx - itemIndex) * itemTotalHeight - 20 then
                        [ ( "transform", translateY -itemTotalHeight )
                        , ( "transition", "transform 200ms ease-in-out" )
                        ]
                    else if idx /= itemIndex then
                        [ ( "transition", "transform 200ms ease-in-out" ) ]
                    else
                        []

                Nothing ->
                    []
    in
    div [ style <| listItem ++ moveStyle ++ makingWayStyle, onMouseDown <| DragStart idx ]
        [ div [ style itemText ] [ text item ]
        ]


translateX : Int -> String
translateX n =
    "translateX(" ++ px n ++ ")"


translateY : Int -> String
translateY n =
    "translateY(" ++ px n ++ ")"


onMouseDown : (Position -> msg) -> Attribute msg
onMouseDown msg =
    on "mousedown" (Decode.map msg Mouse.position)


translateTo1d : Int -> Int -> Position -> Position
translateTo1d width index pos =
    { y = 0
    , x = pos.x * width + pos.y
    }


translateTo2d : Int -> Int -> Position -> Position
translateTo2d width index pos =
    pos



-- STYLE


type alias StyleList =
    List ( String, String )



-- for page container (root element)


pageContainer : StyleList
pageContainer =
    [ ( "width", "360px" )
    , ( "margin", "auto" )
    , ( "padding", "0 0 8px 0" )
    , ( "backgroundColor", "#fafafa" )
    , ( "fontFamily", "sans-serif" )
    ]



-- for list header (with title)


listHeader : StyleList
listHeader =
    [ ( "display", "flex" )
    , ( "padding", "8px" )
    , ( "margin", "8px 0" )
    ]



-- for title in header


headerTitle : StyleList
headerTitle =
    [ ( "flex", "1 0 auto" )
    , ( "margin", "0" )
    ]



-- for list container


listContainer : StyleList
listContainer =
    [ ( "transformStyle", "preserve-3d" )
    , ( "padding", "0" )
    , ( "margin", "8px 0" )
    ]



-- for list item


itemTotalHeight =
    50


itemTotalWidth =
    100


itemMargin =
    8


itemPadding =
    8


itemBorder =
    1


itemHeight =
    itemTotalHeight - (itemMargin * 2) - (itemBorder * 2)


itemWidth =
    itemTotalWidth - (itemMargin * 2) - (itemBorder * 2)


listItem : StyleList
listItem =
    [ ( "listStyleType", "none" )
    , ( "margin", px itemMargin )
    , ( "padding", px itemPadding )

    --     , ( "height", "24px" )
    , ( "backgroundColor", "white" )
    , ( "border", toString itemBorder ++ "px solid rgba(0,0,0,.27)" )
    , ( "border-radius", "2px" )
    , ( "box-shadow", "0 1px 2px rgba(0,0,0,0.24)" )
    , ( "display", "flex" )
    , ( "width", px itemWidth )
    , ( "height", px itemHeight )
    , ( "float", "left" )
    ]


px : Int -> String
px n =
    toString n ++ "px"



-- for text in list item container


itemText : StyleList
itemText =
    [ ( "flex", "1 0 auto" )
    , ( "display", "inline-block" )
    ]
