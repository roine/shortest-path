module Model
    exposing
        ( Model
        , initialModel
        , Algo(..)
        , Direction(..)
        )

import Time exposing (Time)
import Dict exposing (Dict)
import Set exposing (Set)
import Navigation
import Route exposing (Route)
import Page.Bfs.Model as BfsModel exposing (Tile)
import Page.Dijkstra.Model as DijkstraModel


type alias Model =
    { bfs : BfsModel.Model
    , dijkstra : DijkstraModel.Model
    , history : Maybe Route
    }


initialModel : Model
initialModel =
    { bfs = BfsModel.initialModel
    , dijkstra = DijkstraModel.initialModel
    , history = Nothing
    }


type Direction
    = West
    | North
    | East
    | South


type Algo
    = Bfs
