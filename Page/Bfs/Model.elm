module Page.Bfs.Model exposing (..)

import Time exposing (Time)
import Dict exposing (Dict)
import Set exposing (Set)


type Direction
    = West
    | North
    | East
    | South


type Status
    = New
    | Running
    | Paused
    | Found (Dict ( Int, Int ) ( Int, Int ))
    | NotFound


type Tile
    = Wall
    | Empty
    | Start
    | End
    | Visited
    | Edge
    | Path


type alias Model =
    { settings : Settings
    , points : Dict ( Int, Int ) Tile
    , edges : Set ( Int, Int )
    , status : Status
    , walls : Set ( Int, Int )
    , start : ( Int, Int )
    , end : ( Int, Int )
    , visited : Dict ( Int, Int ) Direction
    , seed : Int
    }


initialModel =
    { settings = initialSettings
    , points = Dict.empty
    , edges = Set.empty
    , status = New
    , walls = Set.empty
    , start = ( 0, 0 )
    , end = ( 0, 0 )
    , visited = Dict.empty
    , seed = 1
    }


type alias Settings =
    { density : Int
    , speed : Time
    , mouseTilePosition : Maybe ( ( Int, Int ), Tile )
    }


initialSettings =
    { density = 4
    , speed = Time.millisecond * 100
    , mouseTilePosition = Nothing
    }


mapFound fn status =
    case status of
        Found result ->
            Found (fn result)

        otherwise ->
            otherwise
