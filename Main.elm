module Main exposing (main)

import Dict
import Html exposing (..)
import Html.Attributes exposing (type_)
import Html.Events exposing (onClick)
import Time
import Navigation
import UrlParser as Url exposing ((</>), (<?>), s, int, stringParam, top)
import Route exposing (Route(..), route)
import Model exposing (Model)
import Page.Bfs
import Page.Bfs.Model exposing (Tile(..), Status(..))
import Set


-- MODEL


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        initialModel =
            Model.initialModel
    in
        ( initialModel
        , Cmd.batch [ Cmd.map BfsMsg (Page.Bfs.initialCmd initialModel.bfs) ]
        )



-- UPDATE


type Msg
    = BfsMsg Page.Bfs.Msg
    | UrlChange Navigation.Location
    | NewUrl String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BfsMsg subMsg ->
            let
                ( newModel, cmd ) =
                    Page.Bfs.update subMsg model.bfs
            in
                ( { model | bfs = newModel }, Cmd.map BfsMsg cmd )

        UrlChange location ->
            ( { model | history = Url.parsePath route location }
            , Cmd.none
            )

        NewUrl url ->
            ( model
            , Navigation.newUrl url
            )



-- VIEW


view : Model -> Html Msg
view model =
    let
        bfs =
            model.bfs
    in
        div []
            [ button [ onClick (NewUrl "dijkstra") ] [ text "dijsktra" ]
            , button [ onClick (NewUrl "/") ] [ text "bfs" ]
            , viewRoute model
            ]


viewRoute { history, bfs } =
    case history of
        Nothing ->
            text "Not Found"

        Just route ->
            case route of
                BfsRoute ->
                    Html.map BfsMsg
                        (Page.Bfs.view bfs)

                DijkstraRoute ->
                    text "dijkstra"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Sub.map BfsMsg (Page.Bfs.subscriptions model.bfs) ]



-- INIT


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
