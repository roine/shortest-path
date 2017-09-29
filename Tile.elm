module Tile
    exposing
        ( Tile(..)
        , CurrentTile
        , ParentTile
        , toColour
        , pointToDirection
        , directionToArrow
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
import ColorMath
import Color
import Color.Manipulate
import Color.Convert
import Color.Blending


-- Tile


type Tile
    = Wall
    | Empty Weight
    | Start
    | End Weight
    | Visited Weight
    | Edge Weight
    | Path Weight


type alias Weight =
    Int


type alias CurrentTile =
    ( Int, Int )


type alias ParentTile =
    ( Int, Int )


toColour : Tile -> String
toColour tile =
    case tile of
        Wall ->
            "url(#wall)"

        Empty weight ->
            if weight == 1 then
                "#fff"
            else
                Color.Manipulate.weightedMix (Color.rgb 77 54 25) (Color.rgb 183 229 0) (toFloat weight / 10)
                    |> Color.Convert.colorToHex

        Start ->
            "#aa6001"

        End weight ->
            "#eb1c24"

        Visited weight ->
            if weight == 1 then
                Color.rgb 228 246 240
                    |> Color.Convert.colorToHex
            else
                Color.Manipulate.weightedMix (Color.rgb 77 54 25) (Color.rgb 183 229 0) (toFloat weight / 10)
                    |> Color.Blending.overlay (Color.rgba 228 246 240 0.6)
                    |> Color.Convert.colorToHex

        Edge _ ->
            "#88AFD7"

        Path _ ->
            "#C1B244"


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


directionToArrow : Direction -> String
directionToArrow direction =
    case direction of
        West ->
            "←"

        East ->
            "→"

        South ->
            "↓"

        North ->
            "↑"


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
                                    Just (Empty 1)

                                Just (Empty _) ->
                                    Just Wall

                                Just (Path _) ->
                                    Just Wall

                                Just (Visited _) ->
                                    Just Wall

                                Just (Edge _) ->
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
            ( { model
                | points = Dict.insert points Wall model.points
                , walls =
                    if Set.member points model.walls then
                        Set.remove points model.walls
                    else
                        Set.insert points model.walls
              }
            , Cmd.none
            )

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
                |> Maybe.withDefault (Empty 1)
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
                    ShowPosition ( ( pointX, pointY ), Maybe.withDefault (Empty 1) <| Dict.get ( pointX, pointY ) points )
            ]
            []
