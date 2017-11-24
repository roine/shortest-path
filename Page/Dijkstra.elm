module Page.Dijkstra exposing (..)

import Dict
import Html exposing (..)
import Page.Dijkstra.Menu as Menu
import Random exposing (Generator)
import Set
import Task
import Page.Dijkstra.Menu as Menu
import Page.Dijkstra.Grid as Grid
import Page.Dijkstra.Model as Model exposing (Model)


initialCmd : Model -> Cmd Msg
initialCmd model =
    Cmd.none


type Msg
    = MenuMsg Menu.Msg
    | GridMsg Grid.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ Html.map MenuMsg (Menu.view model)
        , Html.map GridMsg (Grid.view model)
        ]
