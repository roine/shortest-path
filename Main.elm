module Main exposing (..)

import Dict exposing (Dict)
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
    { points : Dict ( Int, Int ) Tile
    , start : ( Int, Int )
    , end : ( Int, Int )
    }


initialModel : Flags -> Model
initialModel flags =
    { points = Dict.empty
    , start = ( 0, 0 )
    , end = ( 0, 0 )
    }


tileToColour : Tile -> String
tileToColour tile =
    case tile of
        Wall ->
            "grey"

        Empty ->
            "white"

        Start ->
            "#38A849"

        End ->
            "#5037A4"


type Status
    = New
    | Running
    | Found
    | NotFound


type Tile
    = Wall
    | Empty
    | Start
    | End


tileSize : Int
tileSize =
    9


type alias Flags =
    { width : Int, height : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel flags
    , Cmd.batch
        [ Random.generate AddRandomWalls (randomWalls 5)
        , Random.generate AddStartPoint randomStart
        , Random.generate AddEndPoint randomEnd
        ]
    )


dimensions : Int
dimensions =
    50


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



-- UPDATE


type Msg
    = AddStartPoint ( Int, Int )
    | AddEndPoint ( Int, Int )
    | AddRandomWalls (List ( Int, Int ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddStartPoint points ->
            ( { model | points = Dict.insert points Start model.points, start = points }, Cmd.none )

        AddEndPoint points ->
            ( { model | points = Dict.insert points (End) model.points, end = points }, Cmd.none )

        AddRandomWalls listOfPoints ->
            ( { model
                | points =
                    List.map (\points -> ( points, Wall )) listOfPoints
                        |> Dict.fromList
                        |> Dict.union model.points
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    let
        renderRect ( pointX, pointY ) points =
            let
                tileType =
                    Dict.get ( pointX, pointY ) points
                        |> Maybe.withDefault Empty
            in
                rect
                    [ x <| toString (pointX * tileSize)
                    , y <| toString (pointY * tileSize)
                    , width <| toString tileSize
                    , height <| toString tileSize
                    , fill <| tileToColour tileType
                    , stroke "aliceblue"
                    , strokeWidth "1"
                    ]
                    []
    in
        div []
            [ svg
                [ width <| toString <| tileSize * dimensions
                , height <| toString <| tileSize * dimensions
                , Html.Attributes.style [ ( "margin", "0 auto" ), ( "display", "block" ), ( "border", "1px solid lightblue" ) ]
                ]
                (List.map
                    (\y ->
                        List.map (\x -> renderRect ( x, y ) model.points) (List.range 0 (dimensions - 1))
                    )
                    (List.range 0 (dimensions - 1))
                    |> List.concat
                )
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- INIT


main : Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
