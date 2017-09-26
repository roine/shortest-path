module Model
    exposing
        ( Model
        , initialModel
        , Algo(..)
        , Status(..)
        , Direction(..)
        )

import Time exposing (Time)
import Dict exposing (Dict)
import Set exposing (Set)


type alias Model a =
    { isMouseDown : Bool
    , density : Int
    , speed : Time.Time
    , points : Dict ( Int, Int ) a
    , edges : Set ( Int, Int )
    , algorithm : Maybe Algo
    , status : Status
    , walls : Set ( Int, Int )
    , start : ( Int, Int )
    , end : ( Int, Int )
    , visited : Dict ( Int, Int ) Direction
    , mouseTilePosition : ( ( Int, Int ), a )
    }


initialModel : a -> Model a
initialModel tile =
    { density = 4
    , points = Dict.empty
    , speed = Time.millisecond * 100
    , isMouseDown = False
    , edges = Set.empty
    , algorithm = Nothing
    , status = New
    , walls = Set.empty
    , start = ( 0, 0 )
    , end = ( 0, 0 )
    , visited = Dict.empty
    , mouseTilePosition = ( ( 0, 0 ), tile )
    }


type Direction
    = West
    | North
    | East
    | South


type Algo
    = Bfs


type Status
    = New
    | Running
    | Paused
    | Found (Set ( ( Int, Int ), ( Int, Int ) ))
    | NotFound
