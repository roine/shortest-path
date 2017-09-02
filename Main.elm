module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (style, value, selected)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events
import Svg.Lazy
import Dict exposing (Dict)
import Random exposing (Generator)
import Set exposing (Set)
import Time


-- MODEL


type alias Model =
    { isMouseDown : Bool
    , density : Int
    , speed : Time.Time
    , points : Dict ( Int, Int ) Tile
    , edges : Set ( Int, Int )
    , algorithm : Maybe ( Algo, Status )
    , start : ( Int, Int )
    , end : ( Int, Int )
    , path : Set ( Int, Int )
    }


initialModel : Flags -> Model
initialModel flags =
    { density = initialWallDensity
    , points = Dict.empty
    , speed = Time.millisecond * 100
    , isMouseDown = False
    , edges = Set.empty
    , algorithm = Nothing
    , start = ( 0, 0 )
    , end = ( 0, 0 )
    , path = Set.empty
    }


tileToColour : Tile -> String
tileToColour tile =
    case tile of
        Wall ->
            "url(#wall)"

        Empty ->
            "white"

        Start ->
            "#38A849"

        End _ ->
            "#5037A4"

        Trail _ ->
            "#E4F6F0"

        Edge _ ->
            "#88AFD7"

        Path ->
            "#C76957"


type Algo
    = Bfs


type Status
    = New
    | Running
    | Found
    | NotFound


type Tile
    = Wall
    | Empty
    | Start
    | End (Set ( Int, Int ))
    | Trail ( Int, Int )
    | Edge ( Int, Int )
    | Path


tiles : Int
tiles =
    80


tileSize : Int
tileSize =
    9


walls : Int -> Int
walls density =
    tiles * (round ((toFloat density) / 10 * (toFloat tiles)))


randomStart : Generator ( Int, Int )
randomStart =
    Random.pair (Random.int 0 (tiles - 1)) (Random.int 0 (tiles - 1))


randomEnd : Generator ( Int, Int )
randomEnd =
    Random.pair (Random.int 0 (tiles - 1)) (Random.int 0 (tiles - 1))


randomWalls : Int -> Generator (List ( Int, Int ))
randomWalls density =
    Random.list (walls density) <| Random.pair (Random.int 0 (tiles - 1)) (Random.int 0 (tiles - 1))


type alias Flags =
    { width : Int, height : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel flags
    , draw { wallDensity = initialWallDensity }
    )


initialWallDensity : Int
initialWallDensity =
    1


draw : { wallDensity : Int } -> Cmd Msg
draw { wallDensity } =
    Cmd.batch
        [ Random.generate AddRandomWalls (randomWalls wallDensity)
        , Random.generate AddStartPoint randomStart
        , Random.generate AddEndPoint randomEnd
        ]



-- UPDATE


type Msg
    = ToggleWall ( Int, Int )
    | AddWall ( Int, Int )
    | MouseDown Bool
    | AddStartPoint ( Int, Int )
    | AddEndPoint ( Int, Int )
    | AddRandomWalls (List ( Int, Int ))
    | ChangeWallDensity Int
    | Redraw
    | RunAlgo Algo
    | RunBfs
    | ChangeSpeed Float
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleWall points ->
            ( { model
                | points =
                    case Dict.get points model.points of
                        Nothing ->
                            Dict.insert points Wall model.points

                        Just _ ->
                            Dict.remove points model.points
              }
            , Cmd.none
            )

        MouseDown bool ->
            ( { model | isMouseDown = bool }, Cmd.none )

        AddWall points ->
            ( { model | points = Dict.insert points Wall model.points }, Cmd.none )

        AddStartPoint points ->
            ( { model | points = Dict.insert points Start model.points, start = points }, Cmd.none )

        AddEndPoint points ->
            ( { model | points = Dict.insert points (End Set.empty) model.points, end = points }, Cmd.none )

        AddRandomWalls listOfPoints ->
            ( { model
                | points =
                    List.map (\points -> ( points, Wall )) listOfPoints
                        |> Dict.fromList
                        |> Dict.union model.points
              }
            , Cmd.none
            )

        ChangeWallDensity percentage ->
            ( { model
                | points = Dict.empty
                , algorithm = Nothing
                , density = percentage
              }
            , draw { wallDensity = percentage }
            )

        Redraw ->
            ( { model | points = Dict.empty, algorithm = Nothing }, draw { wallDensity = model.density } )

        RunAlgo algo ->
            ( case algo of
                Bfs ->
                    runBfs model
            , Cmd.none
            )

        RunBfs ->
            ( { model
                | algorithm =
                    case model.algorithm of
                        Just _ ->
                            Nothing

                        Nothing ->
                            Just ( Bfs, New )
                , edges = Set.fromList [ model.start ]
              }
            , Cmd.none
            )

        ChangeSpeed speed ->
            ( { model | speed = Time.millisecond * speed }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


runBfs : Model -> Model
runBfs model =
    let
        newPoints =
            List.concatMap
                (\edge ->
                    List.map
                        (\( end, newEdge ) ->
                            if end then
                                case Dict.get edge model.points of
                                    Just (Edge parentEdge) ->
                                        ( newEdge, (End (getPath parentEdge <| Set.fromList [ edge ])) )

                                    _ ->
                                        ( newEdge, (Edge edge) )
                            else
                                ( newEdge, (Edge edge) )
                        )
                        (findEdges edge model.points)
                )
                (Set.toList model.edges)

        updateOldEdges points =
            List.foldl
                (\pos acc ->
                    Dict.update pos
                        (\maybe ->
                            case maybe of
                                Just (Edge parent) ->
                                    Just (Trail parent)

                                _ ->
                                    maybe
                        )
                        acc
                )
                points
                (Set.toList model.edges)

        newEdges =
            List.map Tuple.first newPoints

        newAlgorithm =
            if List.isEmpty newEdges then
                Just ( Bfs, NotFound )
            else if foundEnd then
                Just ( Bfs, Found )
            else
                model.algorithm

        foundEnd =
            case Dict.get model.end (newPoints |> Dict.fromList) of
                Nothing ->
                    False

                _ ->
                    True

        addPath points =
            case Dict.get model.end points of
                Nothing ->
                    points

                Just (End parents) ->
                    List.foldl (\point acc -> Dict.insert point Path acc) points (Set.toList parents)

                _ ->
                    points

        getPath point acc =
            case Dict.get point model.points of
                Just (Trail parentPoint) ->
                    getPath parentPoint (Set.insert point acc)

                Just Start ->
                    acc

                _ ->
                    acc
    in
        { model
            | points =
                newPoints
                    |> Dict.fromList
                    |> (\newPoints ->
                            Dict.union newPoints model.points
                       )
                    |> addPath
                    |> updateOldEdges
            , edges = newEdges |> Set.fromList
            , algorithm = newAlgorithm
        }



--


findEdges : ( Int, Int ) -> Dict ( Int, Int ) Tile -> List ( Bool, ( Int, Int ) )
findEdges ( x1, y1 ) points =
    [ ( x1 + 1, y1 ), ( x1 - 1, y1 ), ( x1, y1 - 1 ), ( x1, y1 + 1 ) ]
        |> List.filterMap
            (\point ->
                case Dict.get point points of
                    Nothing ->
                        Just ( False, point )

                    Just Empty ->
                        Just ( False, point )

                    Just (End path) ->
                        Just ( True, point )

                    _ ->
                        Nothing
            )
        |> List.filter
            (\( end, ( x1, y1 ) ) ->
                not (x1 < 0 || y1 < 0 || x1 >= tiles || y1 >= tiles)
            )



-- VIEW


view : Model -> Html Msg
view model =
    let
        renderRect ( ( pointX, pointY ), tile ) =
            rect
                [ x <| toString (pointX * tileSize)
                , y <| toString (pointY * tileSize)
                , width <| toString tileSize
                , height <| toString tileSize
                , fill <| tileToColour tile
                , stroke "aliceblue"
                , strokeWidth "1"
                , Svg.Events.onClick (ToggleWall ( pointX, pointY ))
                , Svg.Events.onMouseOver <|
                    if model.isMouseDown then
                        (AddWall ( pointX, pointY ))
                    else
                        NoOp
                ]
                []

        tile x y =
            Dict.get ( x, y ) model.points
                |> Maybe.withDefault Empty
    in
        div []
            [ svg
                [ width <| toString <| tileSize * tiles
                , height <| toString <| tileSize * tiles
                , Html.Attributes.style [ ( "margin", "0 auto" ), ( "display", "block" ), ( "border", "1px solid lightblue" ) ]
                , Svg.Events.onMouseDown (MouseDown True)
                , Svg.Events.onMouseUp (MouseDown False)
                ]
                ([ defs []
                    [ linearGradient
                        [ id "wall"
                        , x1 "1"
                        , x2 "1"
                        , y1 "1"
                        , y2 "0"
                        ]
                        [ stop [ offset "0%", stopColor "#888" ] []
                        , stop [ offset "49%", stopColor "#ccc" ] []
                        , stop [ offset "100%", stopColor "#aaa" ] []
                        ]
                    ]
                 ]
                    ++ (List.map
                            (\y ->
                                List.map (\x -> Svg.Lazy.lazy renderRect ( ( x, y ), tile x y )) (List.range 0 (tiles - 1))
                            )
                            (List.range 0 (tiles - 1))
                            |> List.concat
                       )
                )
            , div [ menuStyle ]
                [ div []
                    [ span [] [ Html.text "Wall Density" ]
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
                    ]
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
                                option [] [ Html.text <| toString <| speed * 100 ]
                            )
                            (List.range 1 10)
                        )
                    ]
                , div []
                    [ button [ onClick Redraw ] [ Html.text "Redraw" ] ]
                , div [] [ button [ onClick RunBfs ] [ Html.text "Start BFS" ] ]
                , case model.algorithm of
                    Nothing ->
                        Html.text ""

                    Just algo ->
                        div [] [ Html.text <| toString <| Tuple.second algo ]
                ]
            ]


menuStyle : Html.Attribute Msg
menuStyle =
    Html.Attributes.style
        [ ( "position", "absolute" )
        , ( "top", "0" )
        , ( "left", "0" )
        , ( "padding", "10px" )
        , ( "background", "#ddd" )
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.algorithm of
        Nothing ->
            Sub.none

        Just ( Bfs, Found ) ->
            Sub.none

        Just ( Bfs, NotFound ) ->
            Sub.none

        Just ( algo, _ ) ->
            Time.every model.speed (always (RunAlgo algo))



-- INIT


main : Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
