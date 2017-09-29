module Algo.Dijkstra exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (disabled, style)
import Random exposing (Generator)
import Tile exposing (Tile(..))
import Dict
import Grid
import Model exposing (Model)


type Msg
    = GenerateObstacles
    | AddObstacles (List ( ( Int, Int ), Int ))


update : Msg -> Model Tile -> ( Model Tile, Cmd Msg )
update msg model =
    case msg of
        GenerateObstacles ->
            ( model, Random.generate AddObstacles randomObstacles )

        AddObstacles obstacles ->
            ( { model
                | points =
                    List.foldl
                        (\obstacle points ->
                            Dict.update (Tuple.first obstacle)
                                (\maybeTile ->
                                    case maybeTile of
                                        Nothing ->
                                            Just (Empty (Tuple.second obstacle))

                                        Just (Empty _) ->
                                            Just (Empty (Tuple.second obstacle))

                                        otherwise ->
                                            otherwise
                                )
                                points
                        )
                        model.points
                        obstacles
              }
            , Cmd.none
            )


randomObstacles : Generator (List ( ( Int, Int ), Int ))
randomObstacles =
    Random.list (Grid.width_ * 10) (Random.pair (Random.pair (Random.int 0 Grid.width_) (Random.int 0 Grid.height_)) (Random.int 1 10))


viewMenu : Model Tile -> Html Msg
viewMenu model =
    div []
        [ h1 [] [ text "Dijkstra" ]
        , button [ onClick GenerateObstacles ] [ text "Add Obstacles" ]
        , div []
            (List.concat
                [ [ text "1" ]
                , (List.map
                    (\n ->
                        span
                            [ style
                                [ ( "background", Tile.toColour (Empty n) )
                                , ( "width", "10px" )
                                , ( "height", "10px" )
                                , ( "display", "inline-block" )
                                ]
                            ]
                            []
                    )
                    (List.range 1 10)
                  )
                , [ text "10" ]
                ]
            )
        ]
