module Main exposing (..)

import Html exposing (Html, button, text, div)
import Html.Events exposing (onClick)
import Route exposing (Route)
import Util exposing ((=>))
import Page.Home as Home
import Page.About as About
import Page.Errored as Errored exposing (PageLoadError)
import Navigation exposing (Location)


main =
    Navigation.program (Route.fromLocation >> SetRoute)
        { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { page : Page }


type Page
    = Home
    | About


init : Location -> ( Model, Cmd Msg )
init location =
    setRoute
        (Route.fromLocation location)
        { page = Home }



-- UPDATE


type Msg
    = SetRoute (Maybe Route)


setRoute : Maybe Route -> Model -> ( Model, Cmd Msg )
setRoute maybeRoute model =
    case maybeRoute of
        Nothing ->
            ( model, Cmd.none )

        Just (Route.Home) ->
            ( { model | page = Home }, Cmd.none )

        Just (Route.About) ->
            ( { model | page = About }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( SetRoute route, _ ) ->
            setRoute route model



-- VIEW


view : Model -> Html Msg
view model =
    case model.page of
        Home ->
            Home.view

        About ->
            About.view



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
