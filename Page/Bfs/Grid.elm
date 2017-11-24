module Page.Bfs.Grid exposing (Msg, update, isWithin, dimensions, width_, height_, view, addRandom)

import Random exposing (Generator)
import Dict exposing (Dict)
import Set exposing (Set)
import Svg exposing (..)
import Svg.Attributes exposing (height, stopColor, offset, y1, y2, x1, x2, id, width)
import Svg.Events exposing (onMouseUp)
import Html exposing (..)
import Html.Attributes
import Page.Bfs.Tile as Tile
import Page.Bfs.Model as Model exposing (Model, Tile(..))
import Style.Tile as Style
import Style.Grid as Style


dimensions : Int
dimensions =
    width_ * height_


width_ =
    100


height_ =
    50


addRandom =
    AddRandomPoint



-- UPDATE


type Msg
    = AddRandomPoint Int
    | TileMsg Tile.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddRandomPoint seed ->
            let
                pointsEnd =
                    randomSinglePoint seed

                pointsStart =
                    randomSinglePoint (seed + 1)

                wallsSeed =
                    seed + 2

                walls : Int -> Int
                walls density =
                    toFloat (density * dimensions) / 10 |> round

                randomWalls : Int -> Generator (List ( Int, Int ))
                randomWalls density =
                    Random.list (walls density) <| Random.pair (Random.int 0 (width_ - 1)) (Random.int 0 (height_ - 1))

                randomWallsSeed =
                    Random.step (randomWalls model.settings.density) (Random.initialSeed wallsSeed)

                wallsPoints =
                    Tuple.first randomWallsSeed

                newPoints =
                    Dict.insert pointsEnd (End) model.points
                        |> Dict.insert pointsStart Start
                        |> (\p ->
                                List.map (\points -> ( points, Wall )) wallsPoints
                                    |> Dict.fromList
                                    |> Dict.union p
                           )
            in
                ( { model
                    | points = newPoints
                    , end = pointsEnd
                    , start = pointsStart
                    , edges = Set.fromList [ pointsStart ]
                    , seed = seed
                    , walls =
                        Set.fromList wallsPoints
                            |> Set.remove pointsStart
                            |> Set.remove pointsEnd
                  }
                , Cmd.none
                )

        TileMsg subMsg ->
            let
                ( newModel, cmd ) =
                    Tile.update subMsg model
            in
                ( newModel, Cmd.map TileMsg cmd )


randomSinglePoint : Int -> ( Int, Int )
randomSinglePoint seed =
    let
        randomPoint : Generator ( Int, Int )
        randomPoint =
            Random.pair (Random.int 0 (width_ - 1)) (Random.int 0 (height_ - 1))
    in
        Random.step randomPoint (Random.initialSeed seed)
            |> Tuple.first


isWithin : ( Int, Int ) -> Bool
isWithin ( pointX, pointY ) =
    pointX >= 0 && pointY >= 0 && pointX <= width_ - 1 && pointY <= height_ - 1



-- VIEW


view : Model -> Svg Msg
view model =
    let
        widthList =
            (List.range 1 width_)

        heightList =
            (List.range 1 height_)
    in
        div [ Style.gridMain ]
            [ svg
                [ width <| toString <| Tile.dimensions * width_
                , height <| toString <| Tile.dimensions * height_
                , Html.Attributes.map never Style.tile
                ]
                ([ defs []
                    [ Svg.map never <| Style.gradientWall "wall" ]
                 ]
                    ++ (List.concatMap
                            (\y ->
                                List.map
                                    (\x ->
                                        Svg.map TileMsg <| Tile.view ( x - 1, y - 1 ) model
                                    )
                                    widthList
                            )
                            heightList
                       )
                )
            ]
