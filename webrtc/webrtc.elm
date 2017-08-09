port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Events.Extra exposing (onEnter)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { id : String
    , input : ( Id, PeerId, Message )
    , messages : List ( String, String )
    , peers : List String
    }


type alias Id =
    String


type alias PeerId =
    String


type alias Message =
    String


init =
    let
        model =
            { id = ""
            , input = ( "", "", "" )
            , messages = []
            , peers = []
            }
    in
    ( model, Cmd.none )



-- UPDATE


type Msg
    = Input Input
    | ConnectPeer
    | SendData
    | SendId
    | RecvData ( String, String )
    | NewID String


type Input
    = MessageInput String
    | IdInput String
    | PeerInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input input ->
            updateInput input model

        SendId ->
            let
                ( id, _, _ ) =
                    model.input
            in
            ( model, createPeer id )

        ConnectPeer ->
            let
                ( _, peerid, _ ) =
                    model.input
            in
            ( model, connectPeer peerid )

        SendData ->
            let
                ( _, _, msg ) =
                    model.input
            in
            ( model, sendData msg )

        RecvData ( id, data ) ->
            ( { model | messages = model.messages ++ [ ( id, data ) ] }, Cmd.none )

        NewID id ->
            ( { model | id = id }, Cmd.none )


updateInput : Input -> Model -> ( Model, Cmd Msg )
updateInput inputMsg model =
    case inputMsg of
        MessageInput msg ->
            let
                ( id, peerid, _ ) =
                    model.input
            in
            ( { model | input = ( id, peerid, msg ) }, Cmd.none )

        IdInput id ->
            let
                ( _, peerid, msg ) =
                    model.input
            in
            ( { model | input = ( id, peerid, msg ) }, Cmd.none )

        PeerInput peerid ->
            let
                ( id, _, msg ) =
                    model.input
            in
            ( { model | input = ( id, peerid, msg ) }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text ("ID: " ++ model.id) ]
        , idInput
        , peerInput
        , messageInput
        , div [] (List.map viewMessage model.messages)
        ]


viewMessage : ( String, String ) -> Html Msg
viewMessage ( id, msg ) =
    div [] [ Html.text (id ++ ": " ++ msg) ]


idInput : Html Msg
idInput =
    div []
        [ input
            [ onInput (IdInput >> Input)
            ]
            []
        , button [ onClick SendId ] [ text "Create" ]
        ]


peerInput : Html Msg
peerInput =
    div []
        [ input
            [ onInput (PeerInput >> Input)
            ]
            []
        , button [ onClick ConnectPeer ] [ text "Connect" ]
        ]


messageInput : Html Msg
messageInput =
    div []
        [ input
            [ onInput (MessageInput >> Input)
            ]
            []
        , button [ onClick SendData ] [ text "Send" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ recvData RecvData
        , peerID NewID
        ]



-- PORTS


port createPeer : String -> Cmd msg


port connectPeer : String -> Cmd msg


port peerID : (String -> msg) -> Sub msg


port sendData : String -> Cmd msg


port recvData : (( String, String ) -> msg) -> Sub msg
