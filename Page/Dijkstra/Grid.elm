module Page.Dijkstra.Grid exposing (..)

import Html exposing (div)
import Html.Attributes
import Svg exposing (Svg, defs, svg)
import Svg.Attributes exposing (height, width)
import Page.Dijkstra.Tile as Tile
import Style.Grid as Style
import Style.Tile as Style
import Page.Dijkstra.Model exposing (Model)
import Page.Dijkstra.Tile as Tile


width_ =
    100


height_ =
    50


type Msg
    = TileMsg Tile.Msg


update msg model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Svg Msg
view model =
    let
        widthList =
            (List.range 1 width_)

        heightList =
            (List.range 1 height_)
    in
        div [ Style.gridMain ]
            [ svg
                [ width <| toString <| Tile.dimensions * width_
                , height <| toString <| Tile.dimensions * height_
                , Html.Attributes.map never Style.tile
                ]
                ([ defs []
                    [ Svg.map never <| Style.gradientWall "wall" ]
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
