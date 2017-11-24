module Style.Tile exposing (..)

import Color
import Color.Convert
import Html exposing (Attribute)
import Html.Attributes exposing (style)
import Svg exposing (Svg, linearGradient, stop)
import Svg.Attributes exposing (id, offset, stopColor, x1, x2, y1, y2)


empty =
    "#fff"


start =
    "#aa6001"


end =
    "#eb1c24"


visited =
    Color.rgb 228 246 240
        |> Color.Convert.colorToHex


edge =
    "#88AFD7"


path =
    "#C1B244"


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


tile : Attribute Never
tile =
    style
        [ ( "margin", "0 auto" )
        , ( "display", "block" )
        , ( "border", "1px solid lightblue" )
        ]
