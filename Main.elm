module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (type_)
import Html.Events exposing (onClick)
import Time
import Model exposing (Model, Algo(..), Status(..))
import Algo.Bfs as Bfs
import Menu
import Grid
import Tile exposing (Tile(..))


-- MODEL


init : ( Model Tile, Cmd Msg )
init =
    ( Model.initialModel Empty
    , Cmd.map GridMsg (Grid.draw (Model.initialModel Empty))
    )



-- UPDATE


type Msg
    = MenuMsg Menu.Msg
    | GridMsg Grid.Msg
    | BfsMsg Bfs.Msg


update : Msg -> Model Tile -> ( Model Tile, Cmd Msg )
update msg model =
    case msg of
        MenuMsg subMsg ->
            let
                ( newModel, cmd ) =
                    Menu.update subMsg model
            in
                ( newModel, Cmd.map MenuMsg cmd )

        GridMsg subMsg ->
            let
                ( newModel, cmd ) =
                    Grid.update subMsg model
            in
                ( newModel, Cmd.map GridMsg cmd )

        BfsMsg subMsg ->
            let
                newModel =
                    Bfs.update subMsg model
            in
                ( newModel, Cmd.none )



-- VIEW


view : Model Tile -> Html Msg
view model =
    div []
        [ Html.map GridMsg (Grid.view model)
        , Html.map MenuMsg (Menu.view model)
        , Html.text <| toString model
        ]



-- SUBSCRIPTIONS


subscriptions : Model Tile -> Sub Msg
subscriptions model =
    case model.algorithm of
        Nothing ->
            Sub.none

        Just algo ->
            case model.status of
                Running ->
                    Sub.map BfsMsg (Bfs.subscriptions model)

                _ ->
                    Sub.none



-- INIT


main : Program Never (Model Tile) Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
