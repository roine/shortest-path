module Style.Menu exposing (..)

import Html.Attributes exposing (style)
import Style.Tile


menuHead =
    Html.Attributes.style []


menuStart =
    style
        [ ( "background", Style.Tile.start )
        , ( "color", "#fff" )
        , ( "padding", "10px" )
        ]


menuEnd =
    style
        [ ( "background", Style.Tile.end )
        , ( "color", "#fff" )
        , ( "padding", "10px" )
        ]


menuBody =
    style [ ( "padding", "10px" ) ]


menu =
    style
        [ ( "background", "#f5f5f7" )
        , ( "width", "15vw" )
        , ( "box-sizing", "border-box" )
        , ( "display", "inline-block" )
        , ( "box-shadow", "0 2px 3px rgba(0,0,0,0.5)" )
        , ( "border-radius", "0 0 3px 0" )
        ]
