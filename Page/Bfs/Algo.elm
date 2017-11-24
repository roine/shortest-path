module Page.Bfs.Algo exposing (Msg, update, viewMenu, subscriptions)

import Dict exposing (Dict)
import Set exposing (Set)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (disabled)
import Time exposing (Time)
import Task
import Page.Bfs.Model as Model
    exposing
        ( Model
        , Status(..)
        , Direction(..)
        , Tile(..)
        )
import Page.Bfs.Tile as Tile exposing (CurrentTile, ParentTile)
import Page.Bfs.Grid as Grid


type Msg
    = Instant
    | Step
    | Play
    | Pause
    | SetStartTime Time
    | SetEndTime Time


update : Msg -> Model -> ( Model, Cmd Msg )
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
                                List.foldl
                                    (\visited points ->
                                        Dict.update visited
                                            (\v ->
                                                case v of
                                                    Just Visited ->
                                                        Just (Empty)

                                                    otherwise ->
                                                        otherwise
                                            )
                                            points
                                    )
                                    model.points
                                    (Dict.keys model.visited)
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
                ( newModel, Cmd.none )

        Step ->
            let
                newModel =
                    runBfs model
            in
                ( newModel
                , case newModel.status of
                    Found _ ->
                        Task.perform SetEndTime Time.now

                    _ ->
                        Cmd.none
                )

        Play ->
            ( { model
                | status = Running
              }
            , Task.perform SetStartTime Time.now
            )

        Pause ->
            ( { model | status = Paused }, Cmd.none )

        SetStartTime startTime ->
            let
                time =
                    model.time
            in
                ( { model | time = { time | start = startTime, end = Nothing } }, Cmd.none )

        SetEndTime endTime ->
            let
                time =
                    model.time
            in
                ( { model | time = { time | end = Just endTime } }, Cmd.none )


runBfs : Model -> Model
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
            Set.foldl
                (\newEdge points ->
                    Dict.update newEdge
                        (\maybeTile ->
                            case maybeTile of
                                Just Empty ->
                                    Just (Edge)

                                Just End ->
                                    Just (End)

                                _ ->
                                    Just (Edge)
                        )
                        points
                )
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
                                    Just (Visited)

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
                    List.foldl
                        (\current points ->
                            if current == model.end || current == model.start then
                                points
                            else
                                Dict.insert current (Path) points
                        )
                        points
                        (Dict.keys path)

                _ ->
                    points
    in
        { model
            | edges = newEdges
            , visited = newVisited
            , points = newPoints
            , status = newStatus
        }


{-| Find all accessible Tiles surrounding a Tile
and if any of the found Tile is the end tag it.
Assuming the edge has a wall on top and bottom and the end is its left hand side

    >>> findEdges (1,1) model
    [(False, (2,1)), (True, (0,1))]

-}
findEdges : Model -> ( Int, Int ) -> List ( Int, Int )
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


getPath : ( Int, Int ) -> Dict ( Int, Int ) Direction -> Dict CurrentTile ParentTile
getPath from visited =
    let
        go from acc =
            case Dict.get from visited of
                Just direction ->
                    go (Tile.directionToPoint direction from) (Dict.insert from (Tile.directionToPoint direction from) acc)

                Nothing ->
                    acc
    in
        go from Dict.empty


viewMenu : Model -> Html Msg
viewMenu model =
    div []
        [ div [] [ viewPlayButton model ]
        , div [] [ viewStepButton model ]
        , div [] [ viewInstantButton model ]
        ]


viewInstantButton : Model -> Html Msg
viewInstantButton model =
    button [ onClick Instant ] [ text "Instant" ]


viewStepButton : Model -> Html Msg
viewStepButton model =
    button [ onClick Step, disabled <| hasFound model.status ] [ Html.text "Step by step" ]


viewPlayButton : Model -> Html Msg
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every model.settings.speed (always Step)
