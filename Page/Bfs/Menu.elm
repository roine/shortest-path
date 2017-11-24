module Page.Bfs.Menu exposing (Msg, update, view)

import Dict
import Html exposing (..)
import Html.Attributes exposing (value, selected, style)
import Html.Events exposing (onInput, onClick)
import Time
import Page.Bfs.Model exposing (Model, initialModel, Tile(..), Status(..))
import Page.Bfs.Grid as Grid
import Page.Bfs.Tile as Tile
import Page.Bfs.Algo as Bfs
import Set
import Task


-- UPDATE


type Msg
    = ChangeSpeed Float
    | Redraw (Maybe Int)
    | ChangeWallDensity Int
    | GridMsg Grid.Msg
    | BfsMsg Bfs.Msg
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeSpeed speed ->
            let
                settings =
                    model.settings
            in
                ( { model | settings = { settings | speed = Time.millisecond * speed } }, Cmd.none )

        Redraw maybeSeed ->
            ( { model
                | points = .points initialModel
                , status = .status initialModel
                , edges = .edges initialModel
                , visited = .visited initialModel
              }
            , case maybeSeed of
                Nothing ->
                    Cmd.batch [ Task.perform (always (GridMsg (Grid.addRandom (model.seed + 1)))) (Task.succeed ()) ]

                Just seed ->
                    Cmd.batch [ Task.perform (always (GridMsg (Grid.addRandom seed))) (Task.succeed ()) ]
            )

        ChangeWallDensity percentage ->
            let
                settings =
                    model.settings
            in
                ( { model
                    | points = Dict.empty
                    , settings =
                        { settings | density = percentage }
                  }
                , Cmd.batch [ Task.perform (always (GridMsg (Grid.addRandom model.seed))) (Task.succeed ()) ]
                )

        GridMsg subMsg ->
            let
                ( newModel, cmd ) =
                    Grid.update subMsg model
            in
                ( newModel, Cmd.map GridMsg cmd )

        BfsMsg subMsg ->
            let
                ( newModel, cmd ) =
                    Bfs.update subMsg model
            in
                ( newModel, Cmd.map BfsMsg cmd )

        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ menuStyle ]
        [ div [ menuHeadStyle ]
            [ div
                [ style
                    [ ( "background", Tile.toColour Start )
                    , ( "color", "#fff" )
                    , ( "padding", "10px" )
                    ]
                ]
                [ text "Home" ]
            , div
                [ style
                    [ ( "background", Tile.toColour (End) )
                    , ( "color", "#fff" )
                    , ( "padding", "10px" )
                    ]
                ]
                [ text "Work" ]
            ]
        , div [ menuBodyStyle ]
            [ h1 [] [ Html.text "Configuration" ]
            , span [] [ Html.text "Wall Density" ]
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
                        option [ value <| toString pc, selected (model.settings.density == pc) ]
                            [ Html.text <| toString (pc * 10) ]
                    )
                    (List.range 0 7)
                )
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
                            option [ value <| toString speed, selected (model.settings.speed == speed) ] [ Html.text <| toString <| speed ]
                        )
                        [ 1, 100, 300, 500, 800, 1000, 60000 ]
                    )
                ]
            , div []
                [ button [ onClick (Redraw Nothing) ] [ Html.text "Generate" ] ]
            , span []
                [ text "Seed"
                , input
                    [ value <| toString model.seed
                    , onInput
                        (\str ->
                            case String.toInt str of
                                Err _ ->
                                    NoOp

                                Ok seed ->
                                    Redraw (Just seed)
                        )
                    ]
                    []
                ]
            , div []
                [ case model.time.end of
                    Nothing ->
                        text ""

                    Just end ->
                        text <| "Found in " ++ (toString <| (end - model.time.start) / 1000) ++ "s"
                ]
            , div
                [ style
                    [ ( "max-height", "100px" )
                    , ( "overflow", "scroll" )
                    , ( "word-wrap", "break-word" )
                    ]
                ]
                (case model.status of
                    Found result ->
                        let
                            go current acc =
                                case Dict.get current result of
                                    Nothing ->
                                        acc

                                    Just parent ->
                                        go parent ([ directionView parent current ] ++ acc)

                            getDirections =
                                go model.end []

                            directionView parent current =
                                span [] [ Html.text <| Tile.directionToArrow <| Tile.pointToDirection parent current ]
                        in
                            getDirections

                    otherwise ->
                        [ text <| toString otherwise ]
                )
            , Html.map BfsMsg (Bfs.viewMenu model)
            , div []
                [ text "Mouse position: "
                , case model.settings.mouseTilePosition of
                    Nothing ->
                        text "Out"

                    Just mousePosition ->
                        text <| toString mousePosition
                ]
            ]
        ]


menuHeadStyle =
    Html.Attributes.style []


menuBodyStyle =
    Html.Attributes.style [ ( "padding", "10px" ) ]


menuStyle : Html.Attribute Msg
menuStyle =
    Html.Attributes.style
        [ ( "background", "#f5f5f7" )
        , ( "width", "15vw" )
        , ( "box-sizing", "border-box" )
        , ( "display", "inline-block" )
        , ( "box-shadow", "0 2px 3px rgba(0,0,0,0.5)" )
        , ( "border-radius", "0 0 3px 0" )
        ]
