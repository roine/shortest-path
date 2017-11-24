module Main exposing (main)

import Dict
import Html exposing (..)
import Html.Attributes exposing (type_)
import Html.Events exposing (onClick)
import Time
import Navigation
import UrlParser as Url exposing ((</>), (<?>), s, int, stringParam, top)
import Set
import Route exposing (Route(..), route)
import Model exposing (Model)
import Page.Bfs
import Page.Bfs.Model exposing (Tile(..), Status(..))
import Page.Dijkstra


-- MODEL


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        initialModel =
            Model.initialModel

        page =
            Url.parsePath route location
    in
        ( { initialModel | history = page }
        , initialCmd page initialModel
        )


initialCmd page model =
    case page of
        Nothing ->
            Cmd.none

        Just BfsRoute ->
            Cmd.map BfsMsg (Page.Bfs.initialCmd model.bfs)

        Just DijkstraRoute ->
            Cmd.map DijkstraMsg (Page.Dijkstra.initialCmd model.dijkstra)



-- UPDATE


type Msg
    = BfsMsg Page.Bfs.Msg
    | DijkstraMsg Page.Dijkstra.Msg
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

        DijkstraMsg subMsg ->
            let
                ( newModel, cmd ) =
                    Page.Dijkstra.update subMsg model.dijkstra
            in
                ( { model | dijkstra = newModel }, Cmd.map DijkstraMsg cmd )

        UrlChange location ->
            ( { model | history = Url.parsePath route location }
            , initialCmd (Url.parsePath route location) model
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
            [ button [ onClick (NewUrl "/") ] [ text "bfs" ]
            , button [ onClick (NewUrl "dijkstra") ] [ text "dijsktra" ]
            , viewRoute model
            ]


viewRoute { history, bfs, dijkstra } =
    case history of
        Nothing ->
            text "Not Found"

        Just route ->
            case route of
                BfsRoute ->
                    Html.map BfsMsg (Page.Bfs.view bfs)

                DijkstraRoute ->
                    Html.map DijkstraMsg (Page.Dijkstra.view dijkstra)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.history of
        Nothing ->
            Sub.none

        Just BfsRoute ->
            Sub.map BfsMsg (Page.Bfs.subscriptions model.bfs)

        Just DijkstraRoute ->
            Sub.map DijkstraMsg (Page.Dijkstra.subscriptions model.dijkstra)



-- INIT


main : Program Never Model Msg
main =
    Navigation.program UrlChange
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
