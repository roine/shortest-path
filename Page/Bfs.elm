module Page.Bfs exposing (..)

import Dict
import Html exposing (..)
import Page.Bfs.Model as Model exposing (Status(..))
import Page.Bfs.Algo as Bfs
import Page.Bfs.Model as Model exposing (Model, Tile(..))
import Page.Bfs.Grid as Grid
import Page.Bfs.Menu as Menu
import Random exposing (Generator)
import Set
import Task


initialCmd : Model -> Cmd Msg
initialCmd model =
    Task.perform (always (GridMsg (Grid.addRandom model.seed))) (Task.succeed ())


type Msg
    = BfsMsg Bfs.Msg
    | GridMsg Grid.Msg
    | MenuMsg Menu.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BfsMsg subMsg ->
            let
                ( newModel, cmd ) =
                    Bfs.update subMsg model
            in
                ( newModel, Cmd.map BfsMsg cmd )

        GridMsg subMsg ->
            let
                ( newModel, cmd ) =
                    Grid.update subMsg model
            in
                ( newModel, Cmd.map GridMsg cmd )

        MenuMsg subMsg ->
            let
                ( newModel, cmd ) =
                    Menu.update subMsg model
            in
                ( newModel, Cmd.map MenuMsg cmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.status of
        Running ->
            Sub.map BfsMsg (Bfs.subscriptions model)

        _ ->
            Sub.none


view : Model -> Html Msg
view model =
    div []
        [ Html.map MenuMsg (Menu.view model)
        , Html.map GridMsg (Grid.view model)
        ]
