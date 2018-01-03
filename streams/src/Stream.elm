module Stream exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Lazy exposing (..)


type StreamCell a
    = Nil
    | Cons a (LazyStreamCell a)


type alias LazyStreamCell a =
    Lazy (StreamCell a)


empty : LazyStreamCell a
empty =
    lazy <| \() -> Nil


cons : a -> LazyStreamCell a -> LazyStreamCell a
cons x stream =
    lazy <| \() -> Cons x stream


append : LazyStreamCell a -> LazyStreamCell a -> LazyStreamCell a
append s1 s2 =
    lazy <|
        \() ->
            case force s1 of
                Nil ->
                    force s2

                Cons x tail ->
                    Cons x (append tail s2)


head : LazyStreamCell a -> Maybe a
head stream =
    case force stream of
        Nil ->
            Nothing

        Cons x _ ->
            Just x


take : Int -> LazyStreamCell a -> LazyStreamCell a
take n stream =
    lazy <|
        \() ->
            case n of
                0 ->
                    Nil

                _ ->
                    case force stream of
                        Nil ->
                            Nil

                        Cons x streamTail ->
                            Cons x (take (n - 1) streamTail)


drop : Int -> LazyStreamCell a -> LazyStreamCell a
drop n stream =
    lazy <|
        \() ->
            case n of
                0 ->
                    force stream

                _ ->
                    case force stream of
                        Nil ->
                            Nil

                        Cons x streamTail ->
                            force <| take (n - 1) streamTail


foldl : (a -> b -> b) -> b -> LazyStreamCell a -> b
foldl f init stream =
    case force stream of
        Nil ->
            init

        Cons x streamTail ->
            foldl f (f x init) streamTail


reverse : LazyStreamCell a -> LazyStreamCell a
reverse stream =
    foldl cons empty stream



{-------------}


main =
    Html.program
        { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    text "Hello World"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
