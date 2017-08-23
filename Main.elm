module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events
import Svg.Lazy
import Dict exposing (Dict)
import Random


-- MODEL


type alias Model =
    { isMouseDown : Bool
    , points : Dict ( Int, Int ) Tile
    }


tileToColour : Tile -> String
tileToColour tile =
    case tile of
        Wall ->
            "grey"

        Empty ->
            "white"

        Start ->
            "blue"

        End ->
            "green"


type Tile
    = Wall
    | Empty
    | Start
    | End


tiles =
    30


tileSize =
    30


randomStart =
    Random.map2 (,) (Random.int 0 (tiles - 1)) (Random.int 0 (tiles - 1))


randomEnd =
    Random.map2 (,) (Random.int 0 (tiles - 1)) (Random.int 0 (tiles - 1))


type alias Flags =
    { width : Int, height : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel flags
    , Cmd.batch
        [ Random.generate AddStartPoint randomStart
        , Random.generate AddEndPoint randomEnd
        ]
    )


initialModel : Flags -> Model
initialModel flags =
    { points = Dict.empty, isMouseDown = False }



-- UPDATE


type Msg
    = ToggleWall ( Int, Int )
    | AddWall ( Int, Int )
    | MouseDown Bool
    | AddStartPoint ( Int, Int )
    | AddEndPoint ( Int, Int )
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleWall points ->
            ( { model
                | points =
                    case Dict.get points model.points of
                        Nothing ->
                            Dict.insert points Wall model.points

                        Just _ ->
                            Dict.remove points model.points
              }
            , Cmd.none
            )

        MouseDown bool ->
            ( { model | isMouseDown = bool }, Cmd.none )

        AddWall points ->
            ( { model | points = Dict.insert points Wall model.points }, Cmd.none )

        AddStartPoint points ->
            ( { model | points = Dict.insert points Start model.points }, Cmd.none )

        AddEndPoint points ->
            ( { model | points = Dict.insert points End model.points }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []



-- VIEW
--


view : Model -> Html Msg
view model =
    let
        renderRect ( ( pointX, pointY ), tile ) =
            rect
                [ x <| toString (pointX * tileSize)
                , y <| toString (pointY * tileSize)
                , width <| toString tileSize
                , height <| toString tileSize
                , fill <| tileToColour tile
                , stroke "aliceblue"
                , strokeWidth "1"
                , Svg.Events.onClick (ToggleWall ( pointX, pointY ))
                , Svg.Events.onMouseOver <|
                    if model.isMouseDown then
                        (AddWall ( pointX, pointY ))
                    else
                        NoOp
                ]
                []

        tile x y =
            Dict.get ( x, y ) model.points
                |> Maybe.withDefault Empty
    in
        svg
            [ width <| toString <| tileSize * tiles
            , height <| toString <| tileSize * tiles
            , Html.Attributes.style [ ( "margin", "0 auto" ), ( "display", "block" ), ( "border", "1px solid lightblue" ) ]
            , Svg.Events.onMouseDown (MouseDown True)
            , Svg.Events.onMouseUp (MouseDown False)
            ]
            (List.map
                (\y ->
                    List.map (\x -> Svg.Lazy.lazy renderRect ( ( x, y ), tile x y )) (List.range 0 (tiles - 1))
                )
                (List.range 0 (tiles - 1))
                |> List.concat
            )



-- INIT


main =
    programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
