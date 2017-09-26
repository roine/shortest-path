module Grid exposing (Msg, update, draw, isWithin, dimensions, view)

import Random exposing (Generator)
import Dict exposing (Dict)
import Set exposing (Set)
import Svg exposing (..)
import Svg.Attributes exposing (height, stopColor, offset, y1, y2, x1, x2, id, width)
import Svg.Events exposing (onMouseDown, onMouseUp)
import Html
import Html.Attributes
import Tile exposing (Tile(..))
import Model exposing (Status(..), Model)


dimensions : Int
dimensions =
    20



-- UPDATE


type Msg
    = AddStartPoint ( Int, Int )
    | AddEndPoint ( Int, Int )
    | AddRandomWalls (List ( Int, Int ))
    | MouseDown Bool
    | TileMsg Tile.Msg


update : Msg -> Model Tile -> ( Model Tile, Cmd Msg )
update msg model =
    case msg of
        AddStartPoint points ->
            ( { model
                | points = Dict.insert points Start model.points
                , start = points
                , edges = Set.fromList [ points ]
              }
            , Cmd.none
            )

        AddEndPoint points ->
            ( { model
                | points = Dict.insert points End model.points
                , end = points
              }
            , Cmd.none
            )

        AddRandomWalls listOfPoints ->
            ( { model
                | points =
                    List.map (\points -> ( points, Wall )) listOfPoints
                        |> Dict.fromList
                        |> Dict.union model.points
                , walls =
                    Set.fromList listOfPoints
                        |> Set.remove (Debug.log "start" model.start)
                        |> Set.remove model.end
              }
            , Cmd.none
            )

        MouseDown bool ->
            ( { model | isMouseDown = bool }, Cmd.none )

        TileMsg subMsg ->
            let
                ( newModel, cmd ) =
                    Tile.update subMsg model
            in
                ( newModel, Cmd.map TileMsg cmd )


walls : Int -> Int
walls density =
    dimensions * (round ((toFloat density) / 10 * (toFloat dimensions)))


randomStart : Generator ( Int, Int )
randomStart =
    Random.pair (Random.int 0 (dimensions - 1)) (Random.int 0 (dimensions - 1))


randomEnd : Generator ( Int, Int )
randomEnd =
    Random.pair (Random.int 0 (dimensions - 1)) (Random.int 0 (dimensions - 1))


randomWalls : Int -> Generator (List ( Int, Int ))
randomWalls density =
    Random.list (walls density) <| Random.pair (Random.int 0 (dimensions - 1)) (Random.int 0 (dimensions - 1))


draw : { a | density : Int } -> Cmd Msg
draw { density } =
    Cmd.batch
        [ Random.generate AddRandomWalls (randomWalls density)
        , Random.generate AddStartPoint randomStart
        , Random.generate AddEndPoint randomEnd
        ]


isWithin : ( Int, Int ) -> Bool
isWithin ( pointX, pointY ) =
    pointX >= 0 && pointY >= 0 && pointX <= dimensions - 1 && pointY <= dimensions - 1



-- VIEW


view : Model Tile -> Svg Msg
view model =
    let
        points =
            (List.range 1 dimensions)
    in
        svg
            [ width <| toString <| Tile.dimensions * dimensions
            , height <| toString <| Tile.dimensions * dimensions
            , Html.Attributes.map never gridStyle
            , onMouseDown (MouseDown True)
            , onMouseUp (MouseDown False)
            ]
            ([ defs []
                [ Svg.map never <| gradientWall "wall" ]
             ]
                ++ (List.concatMap
                        (\y ->
                            List.map
                                (\x ->
                                    Svg.map TileMsg <| Tile.view ( x - 1, y - 1 ) model
                                )
                                points
                        )
                        points
                   )
            )


gridStyle : Html.Attribute Never
gridStyle =
    Html.Attributes.style
        [ ( "margin", "0 auto" )
        , ( "display", "block" )
        , ( "border", "1px solid lightblue" )
        ]


gradientWall : String -> Svg Never
gradientWall id_ =
    linearGradient
        [ id id_
        , x1 "1"
        , x2 "1"
        , y1 "1"
        , y2 "0"
        ]
        [ stop [ offset "0%", stopColor "#888" ] []
        , stop [ offset "49%", stopColor "#ccc" ] []
        , stop [ offset "100%", stopColor "#aaa" ] []
        ]
