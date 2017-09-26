module Algo.Bfs exposing (Msg, update, viewMenu, subscriptions)

import Dict exposing (Dict)
import Set exposing (Set)
import Model
    exposing
        ( Model
        , Status(..)
        , Algo(..)
        , Direction(..)
        )
import Tile exposing (Tile(..), CurrentTile, ParentTile)
import Grid
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (disabled)
import Time


type Msg
    = Instant
    | Step
    | Play
    | Pause


update : Msg -> Model Tile -> Model Tile
update msg model =
    case msg of
        Instant ->
            let
                newModel =
                    go
                        { model
                            | status = New
                            , visited = Dict.empty
                            , edges = Set.fromList [ model.start ]
                            , points =
                                Dict.fromList
                                    (List.map (\w -> ( w, Tile.Wall )) (Set.toList model.walls)
                                        |> List.append [ ( model.start, Tile.Start ) ]
                                        |> List.append [ ( model.end, Tile.End ) ]
                                    )
                        }

                go model =
                    case model.status of
                        Found _ ->
                            model

                        NotFound ->
                            model

                        _ ->
                            go (runBfs model)
            in
                newModel

        Step ->
            let
                newModel =
                    runBfs model
            in
                newModel

        Play ->
            { model
                | algorithm = Just Bfs
                , status = Running
            }

        Pause ->
            { model | status = Paused }


runBfs : Model Tile -> Model Tile
runBfs model =
    let
        newEdges : Set ( Int, Int )
        newEdges =
            List.concatMap (findEdges model) (Set.toList model.edges)
                |> Set.fromList

        newVisited : Dict ( Int, Int ) Direction
        newVisited =
            List.foldl
                (\parent visited ->
                    List.foldl
                        (\edge visited_ ->
                            Dict.insert edge (Tile.pointToDirection edge parent) visited_
                        )
                        visited
                        (findEdges model parent)
                )
                model.visited
                (Set.toList model.edges)

        newPoints : Dict ( Int, Int ) Tile
        newPoints =
            Set.foldl (\newEdge points -> Dict.insert newEdge Edge points)
                model.points
                newEdges
                |> updateOldEdges
                |> maybeAddPath

        updateOldEdges : Dict ( Int, Int ) Tile -> Dict ( Int, Int ) Tile
        updateOldEdges points =
            List.foldl
                (\pos acc ->
                    Dict.update pos
                        (\maybe ->
                            case maybe of
                                Just Edge ->
                                    Just Visited

                                _ ->
                                    maybe
                        )
                        acc
                )
                points
                (Set.toList model.edges)

        newStatus =
            if Dict.get model.end newVisited /= Nothing then
                Found <| getPath model.end newVisited
            else if Set.isEmpty newEdges then
                NotFound
            else
                model.status

        maybeAddPath : Dict ( Int, Int ) Tile -> Dict ( Int, Int ) Tile
        maybeAddPath points =
            case newStatus of
                Found path ->
                    Set.foldl
                        (\( current, parent ) points ->
                            Dict.insert current Path points
                                |> Dict.insert parent Path
                        )
                        points
                        path

                _ ->
                    points
    in
        { model
            | edges = newEdges
            , visited = newVisited
            , points = newPoints
            , status = newStatus
        }


{-| Find all accessibles Tiles surrounding a Tile
and if any of the found Tile is the end tag it.
Assuming the edge has a wall on top and bottom and the end is its left hand side

    >>> findEdges (1,1) model
    [(False, (2,1)), (True, (0,1))]

-}
findEdges : Model Tile -> ( Int, Int ) -> List ( Int, Int )
findEdges { walls, visited, start } ( x1, y1 ) =
    let
        surroundings =
            [ ( x1 + 1, y1 )
            , ( x1 - 1, y1 )
            , ( x1, y1 - 1 )
            , ( x1, y1 + 1 )
            ]

        surroundings_ =
            [ ( x1 + 1, y1 )
            , ( x1 - 1, y1 )
            , ( x1, y1 - 1 )
            , ( x1, y1 + 1 )
            , ( x1 + 1, y1 + 1 )
            , ( x1 + 1, y1 - 1 )
            , ( x1 - 1, y1 + 1 )
            , ( x1 - 1, y1 - 1 )
            ]
    in
        surroundings
            |> List.filter (\p -> not <| Set.member p walls)
            |> List.filter (\p -> not <| maybeToBool <| Dict.get p visited)
            |> List.filter (\p -> p /= start)
            |> List.filter Grid.isWithin


maybeToBool : Maybe a -> Bool
maybeToBool maybeValue =
    case maybeValue of
        Just _ ->
            True

        Nothing ->
            False


getPath : ( Int, Int ) -> Dict ( Int, Int ) Direction -> Set ( CurrentTile, ParentTile )
getPath from visited =
    let
        go from acc =
            case Dict.get from visited of
                Just direction ->
                    go (Tile.directionToPoint direction from) (Set.insert ( from, Tile.directionToPoint direction from ) acc)

                Nothing ->
                    acc
    in
        go from Set.empty


viewMenu : Model Tile -> Html Msg
viewMenu model =
    div []
        [ h1 [] [ Html.text "Bfs" ]
        , div [] [ viewPlayButton model ]
        , div [] [ viewStepButton model ]
        , div [] [ viewInstantButton model ]
        ]


viewInstantButton : Model Tile -> Html Msg
viewInstantButton model =
    button [ onClick Instant, disabled <| hasFound model.status ] [ text "Instant" ]


viewStepButton : Model Tile -> Html Msg
viewStepButton model =
    button [ onClick Step, disabled <| hasFound model.status ] [ Html.text "Step by step" ]


viewPlayButton : Model Tile -> Html Msg
viewPlayButton model =
    if model.status == Running then
        button [ onClick Pause ] [ Html.text "Pause" ]
    else
        button [ onClick Play, disabled <| hasFound model.status ] [ Html.text "Play" ]


hasFound : Status -> Bool
hasFound status =
    case status of
        Found _ ->
            True

        _ ->
            False


subscriptions : Model Tile -> Sub Msg
subscriptions model =
    Time.every model.speed (always Step)
