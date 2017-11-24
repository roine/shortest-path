module Page.Dijkstra.Menu exposing (..)

import Html exposing (..)
import Style.Menu as Style


type Msg
    = NoOp


update msg model =
    ( model, Cmd.none )


view model =
    div [ Style.menu ]
        [ div [ Style.menuHead ]
            [ div
                [ Style.menuStart
                ]
                [ text "Home" ]
            , div
                [ Style.menuEnd
                ]
                [ text "Work" ]
            ]
        , div [ Style.menuBody ] [ h1 [] [ Html.text "Configuration" ] ]
        ]
