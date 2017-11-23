module Route exposing (Route(..), home, route)

import UrlParser exposing (oneOf, map, s, top)


type Route
    = BfsRoute
    | DijkstraRoute


home =
    Just BfsRoute


route =
    oneOf
        [ map (BfsRoute) top
        , map DijkstraRoute (s "dijkstra")
        ]
