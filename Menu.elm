module Menu exposing (Msg, update, view)

import Dict
import Html exposing (..)
import Html.Attributes exposing (value, selected, style)
import Html.Events exposing (onInput, onClick)
import Time
import Model exposing (Algo(..), Status(..), Model)
import Grid
import Tile exposing (Tile)
import Algo.Bfs as Bfs
import Algo.Dijkstra as Dijkstra
import Set


-- UPDATE


type Msg
    = ChangeSpeed Float
    | Redraw
    | ChangeWallDensity Int
    | GridMsg Grid.Msg
    | BfsMsg Bfs.Msg
    | DijkstraMsg Dijkstra.Msg
    | NoOp


update : Msg -> Model Tile -> ( Model Tile, Cmd Msg )
update msg model =
    case msg of
        ChangeSpeed speed ->
            ( { model | speed = Time.millisecond * speed }, Cmd.none )

        Redraw ->
            ( { model
                | points = .points <| Model.initialModel <| Tile.Empty 1
                , algorithm = .algorithm <| Model.initialModel <| Tile.Empty 1
                , status = .status <| Model.initialModel <| Tile.Empty 1
                , edges = .edges <| Model.initialModel <| Tile.Empty 1
                , visited = .visited <| Model.initialModel <| Tile.Empty 1
              }
            , Cmd.map GridMsg <| Grid.draw model
            )

        ChangeWallDensity percentage ->
            ( { model
                | points = Dict.empty
                , algorithm = Nothing
                , density = percentage
              }
            , Cmd.map GridMsg <| Grid.draw { model | density = percentage }
            )

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

        DijkstraMsg subMsg ->
            let
                ( newModel, cmd ) =
                    Dijkstra.update subMsg model
            in
                ( newModel, Cmd.map DijkstraMsg cmd )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model Tile -> Html Msg
view model =
    div [ menuStyle ]
        [ div [ menuHeadStyle ]
            [ div
                [ style
                    [ ( "background", Tile.toColour Tile.Start )
                    , ( "color", "#fff" )
                    , ( "padding", "10px" )
                    ]
                ]
                [ text "Home" ]
            , div
                [ style
                    [ ( "background", Tile.toColour (Tile.End 1) )
                    , ( "color", "#fff" )
                    , ( "padding", "10px" )
                    ]
                ]
                [ text "Work" ]
            ]
        , div [ menuBodyStyle ]
            [ h1 [] [ Html.text "Configuration" ]
            , span [] [ Html.text "Wall Density" ]
            , select
                [ onInput
                    (\str ->
                        case String.toInt str of
                            Ok int ->
                                ChangeWallDensity int

                            Err _ ->
                                NoOp
                    )
                ]
                (List.map
                    (\pc ->
                        option [ value <| toString pc, selected (model.density == pc) ]
                            [ Html.text <| toString (pc * 10) ]
                    )
                    (List.range 0 7)
                )
            , div []
                [ span [] [ Html.text "Speed (ms)" ]
                , select
                    [ onInput
                        (\str ->
                            case String.toFloat str of
                                Ok float ->
                                    ChangeSpeed float

                                Err _ ->
                                    NoOp
                        )
                    ]
                    (List.map
                        (\speed ->
                            option [ value <| toString speed, selected (model.speed == speed) ] [ Html.text <| toString <| speed ]
                        )
                        [ 1, 100, 300, 500, 800, 1000, 60000 ]
                    )
                ]
            , div []
                [ button [ onClick Redraw ] [ Html.text "Generate" ] ]
            , div [] [ text <| toString model.mouseTilePosition ]
            , div
                [ style
                    [ ( "max-height", "100px" )
                    , ( "overflow", "scroll" )
                    , ( "word-wrap", "break-word" )
                    ]
                ]
                (case model.status of
                    Found result ->
                        let
                            go current acc =
                                case Dict.get current result of
                                    Nothing ->
                                        acc

                                    Just parent ->
                                        go parent ([ directionView parent current ] ++ acc)

                            getDirections =
                                go model.end []

                            directionView parent current =
                                span [] [ Html.text <| Tile.directionToArrow <| Tile.pointToDirection parent current ]
                        in
                            getDirections

                    otherwise ->
                        [ text <| toString otherwise ]
                )
            , Html.map BfsMsg (Bfs.viewMenu model)
            , Html.map DijkstraMsg (Dijkstra.viewMenu model)
            ]
        ]


menuHeadStyle =
    Html.Attributes.style []


menuBodyStyle =
    Html.Attributes.style [ ( "padding", "10px" ) ]


menuStyle : Html.Attribute Msg
menuStyle =
    Html.Attributes.style
        [ ( "background", "#f5f5f7" )
        , ( "width", "15vw" )
        , ( "box-sizing", "border-box" )
        , ( "display", "inline-block" )
        , ( "box-shadow", "0 2px 3px rgba(0,0,0,0.5)" )
        , ( "border-radius", "0 0 3px 0" )
        ]
