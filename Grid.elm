module Grid exposing (Msg, update, draw, isWithin, dimensions, width_, height_, view)

import Random exposing (Generator)
import Dict exposing (Dict)
import Set exposing (Set)
import Svg exposing (..)
import Svg.Attributes exposing (height, stopColor, offset, y1, y2, x1, x2, id, width)
import Svg.Events exposing (onMouseDown, onMouseUp)
import Html exposing (..)
import Html.Attributes
import Tile exposing (Tile(..))
import Model exposing (Status(..), Model)


dimensions : Int
dimensions =
    width_ * height_


width_ =
    100


height_ =
    50



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
                | points = Dict.insert points (End 1) model.points
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
                        |> Set.remove model.start
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


randomStart : Generator ( Int, Int )
randomStart =
    Random.pair (Random.int 0 (width_ - 1)) (Random.int 0 (height_ - 1))


randomEnd : Generator ( Int, Int )
randomEnd =
    Random.pair (Random.int 0 (width_ - 1)) (Random.int 0 (height_ - 1))


walls : Int -> Int
walls density =
    toFloat (density * dimensions) / 10 |> round


randomWalls : Int -> Generator (List ( Int, Int ))
randomWalls density =
    Random.list (walls density) <| Random.pair (Random.int 0 (width_ - 1)) (Random.int 0 (height_ - 1))


draw : { a | density : Int } -> Cmd Msg
draw { density } =
    Cmd.batch
        [ Random.generate AddRandomWalls (randomWalls density)
        , Random.generate AddStartPoint randomStart
        , Random.generate AddEndPoint randomEnd
        ]


isWithin : ( Int, Int ) -> Bool
isWithin ( pointX, pointY ) =
    pointX >= 0 && pointY >= 0 && pointX <= width_ - 1 && pointY <= height_ - 1



-- VIEW


view : Model Tile -> Svg Msg
view model =
    let
        widthList =
            (List.range 1 width_)

        heightList =
            (List.range 1 height_)
    in
        div [ mainStyle ]
            [ svg
                [ width <| toString <| Tile.dimensions * width_
                , height <| toString <| Tile.dimensions * height_
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
                                    widthList
                            )
                            heightList
                       )
                )
            ]


gridStyle : Html.Attribute Never
gridStyle =
    Html.Attributes.style
        [ ( "margin", "0 auto" )
        , ( "display", "block" )
        , ( "border", "1px solid lightblue" )
        ]


mainStyle =
    Html.Attributes.style
        [ ( "width", "85vw" )
        , ( "padding", "10px" )
        , ( "box-sizing", "border-box" )
        , ( "display", "inline-block" )
        , ( "vertical-align", "top" )
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
