module Style.Grid exposing (..)

import Html exposing (Attribute)
import Html.Attributes exposing (style)


gridMain =
    style
        [ ( "width", "85vw" )
        , ( "padding", "10px" )
        , ( "box-sizing", "border-box" )
        , ( "display", "inline-block" )
        , ( "vertical-align", "top" )
        ]
