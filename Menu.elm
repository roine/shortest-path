module Menu exposing (Msg, update, view)

import Dict
import Html exposing (..)
import Html.Attributes exposing (value, selected, style)
import Html.Events exposing (onInput, onClick)
import Time
import Model exposing (Algo(..), Status(..), Model)
import Grid
import Tile exposing (Tile)
import Algo.Bfs as Bfs


-- UPDATE


type Msg
    = ChangeSpeed Float
    | Redraw
    | ChangeWallDensity Int
    | GridMsg Grid.Msg
    | BfsMsg Bfs.Msg
    | NoOp


update : Msg -> Model Tile -> ( Model Tile, Cmd Msg )
update msg model =
    case msg of
        ChangeSpeed speed ->
            ( { model | speed = Time.millisecond * speed }, Cmd.none )

        Redraw ->
            ( { model
                | points = .points <| Model.initialModel Tile.Empty
                , algorithm = .algorithm <| Model.initialModel Tile.Empty
                , status = .status <| Model.initialModel Tile.Empty
                , edges = .edges <| Model.initialModel Tile.Empty
                , visited = .visited <| Model.initialModel Tile.Empty
              }
            , Cmd.map GridMsg <| Grid.draw model
            )

        ChangeWallDensity percentage ->
            ( { model
                | points = Dict.empty
                , algorithm = Nothing
                , density = percentage
              }
            , Cmd.map GridMsg <| Grid.draw { model | density = percentage }
            )

        GridMsg subMsg ->
            let
                ( newModel, cmd ) =
                    Grid.update subMsg model
            in
                ( newModel, Cmd.map GridMsg cmd )

        BfsMsg subMsg ->
            let
                newModel =
                    Bfs.update subMsg model
            in
                ( newModel, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model Tile -> Html Msg
view model =
    div [ menuStyle ]
        [ div [ style [ ( "color", Tile.toColour Tile.Start ) ] ] [ text "Start" ]
        , div [ style [ ( "color", Tile.toColour Tile.End ) ] ] [ text "End" ]
        , div []
            [ span [] [ Html.text "Wall Density" ]
            , select
                [ onInput
                    (\str ->
                        case String.toInt str of
                            Ok int ->
                                ChangeWallDensity int

                            Err _ ->
                                NoOp
                    )
                ]
                (List.map
                    (\pc ->
                        option [ value <| toString pc, selected (model.density == pc) ]
                            [ Html.text <| toString (pc * 10) ]
                    )
                    (List.range 0 7)
                )
            ]
        , div []
            [ span [] [ Html.text "Speed (ms)" ]
            , select
                [ onInput
                    (\str ->
                        case String.toFloat str of
                            Ok float ->
                                ChangeSpeed float

                            Err _ ->
                                NoOp
                    )
                ]
                (List.map
                    (\speed ->
                        option [ value <| toString speed, selected (model.speed == speed) ] [ Html.text <| toString <| speed ]
                    )
                    [ 1, 100, 300, 500, 800, 1000, 60000 ]
                )
            ]
        , div []
            [ button [ onClick Redraw ] [ Html.text "Generate" ] ]
        , div [] [ text <| toString model.mouseTilePosition ]
        , Html.text <| toString model.status
        , Html.map BfsMsg (Bfs.viewMenu model)
        ]


menuStyle : Html.Attribute Msg
menuStyle =
    Html.Attributes.style
        [ ( "position", "absolute" )
        , ( "top", "0" )
        , ( "left", "0" )
        , ( "padding", "10px" )
        , ( "background", "#ddd" )
        , ( "width", "15vw" )
        ]
