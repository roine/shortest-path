module Tile
    exposing
        ( Tile(..)
        , CurrentTile
        , ParentTile
        , toColour
        , pointToDirection
        , directionToPoint
        , dimensions
        , Msg
        , update
        , view
        )

import Set exposing (Set)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events
import Dict exposing (Dict)
import Model exposing (Model, Direction(..))


-- Tile


type Tile
    = Wall
    | Empty
    | Start
    | End
    | Visited
    | Edge
    | Path


type alias CurrentTile =
    ( Int, Int )


type alias ParentTile =
    ( Int, Int )


toColour : Tile -> String
toColour tile =
    case tile of
        Wall ->
            "url(#wall)"

        Empty ->
            "white"

        Start ->
            "#38A849"

        End ->
            "#5037A4"

        Visited ->
            "#E4F6F0"

        Edge ->
            "#88AFD7"

        Path ->
            "#C76957"


pointToDirection : ( Int, Int ) -> ( Int, Int ) -> Direction
pointToDirection ( currentPointX, currentPointY ) ( parentPointX, parentPointY ) =
    case ( parentPointX - currentPointX, parentPointY - currentPointY ) of
        ( -1, 0 ) ->
            West

        ( 1, 0 ) ->
            East

        ( 0, -1 ) ->
            North

        ( 0, 1 ) ->
            South

        _ ->
            Debug.crash "impossible"


directionToPoint : Direction -> ( Int, Int ) -> ( Int, Int )
directionToPoint direction ( currentPointX, currentPointY ) =
    case direction of
        West ->
            ( currentPointX - 1, currentPointY )

        East ->
            ( currentPointX + 1, currentPointY )

        North ->
            ( currentPointX, currentPointY - 1 )

        South ->
            ( currentPointX, currentPointY + 1 )


dimensions : Int
dimensions =
    10



-- UPDATE


type Msg
    = ToggleWall ( Int, Int )
    | MakeWall ( Int, Int )
    | ShowPosition ( ( Int, Int ), Tile )
    | NoOp


update : Msg -> Model Tile -> ( Model Tile, Cmd Msg )
update msg model =
    case msg of
        ToggleWall points ->
            ( { model
                | points =
                    Dict.update
                        points
                        (\maybeTile ->
                            case maybeTile of
                                Just Wall ->
                                    Just Empty

                                Just Empty ->
                                    Just Wall

                                Just Path ->
                                    Just Wall

                                Just Visited ->
                                    Just Wall

                                Just Edge ->
                                    Just Wall

                                Nothing ->
                                    Just Wall

                                otherwise ->
                                    otherwise
                        )
                        model.points
                , walls =
                    if Set.member points model.walls then
                        Set.remove points model.walls
                    else
                        Set.insert points model.walls
              }
            , Cmd.none
            )

        MakeWall points ->
            ( { model | points = Dict.insert points Wall model.points }, Cmd.none )

        ShowPosition point ->
            ( { model | mouseTilePosition = point }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



--VIEW


view : ( Int, Int ) -> Model Tile -> Svg Msg
view ( pointX, pointY ) { points, isMouseDown } =
    let
        tileType =
            Dict.get ( pointX, pointY ) points
                |> Maybe.withDefault Empty
    in
        rect
            [ x <| toString (pointX * dimensions)
            , y <| toString (pointY * dimensions)
            , width <| toString dimensions
            , height <| toString dimensions
            , fill <| toColour tileType
            , stroke "aliceblue"
            , strokeWidth "1"
            , Svg.Events.onClick (ToggleWall ( pointX, pointY ))
            , Svg.Events.onMouseOver <|
                if isMouseDown then
                    MakeWall ( pointX, pointY )
                else
                    ShowPosition ( ( pointX, pointY ), Maybe.withDefault Empty <| Dict.get ( pointX, pointY ) points )
            ]
            []
