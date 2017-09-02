module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (style, value, selected)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events
import Svg.Lazy
import Dict exposing (Dict)
import Random exposing (Generator)
import Set exposing (Set)
import Time


-- MODEL


type alias Model =
    {}


initialModel : Flags -> Model
initialModel flags =
    {}


tileToColour : Tile -> String
tileToColour tile =
    case tile of
        Wall ->
            "url(#wall)"

        Empty ->
            "white"

        Start ->
            "#38A849"

        End ->
            "#5037A4"


type Status
    = New
    | Running
    | Found
    | NotFound


type Tile
    = Wall
    | Empty
    | Start
    | End


tiles : Int
tiles =
    80


tileSize : Int
tileSize =
    9


type alias Flags =
    { width : Int, height : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel flags
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        renderRect ( pointX, pointY ) =
            rect
                [ x <| toString (pointX * tileSize)
                , y <| toString (pointY * tileSize)
                , width <| toString tileSize
                , height <| toString tileSize
                , fill "white"
                , stroke "aliceblue"
                , strokeWidth "1"
                ]
                []
    in
        svg
            [ width <| toString <| tileSize * tiles
            , height <| toString <| tileSize * tiles
            , Html.Attributes.style [ ( "margin", "0 auto" ), ( "display", "block" ), ( "border", "1px solid lightblue" ) ]
            ]
            (List.map
                (\y ->
                    List.map (\x -> Svg.Lazy.lazy renderRect ( x, y )) (List.range 0 (tiles - 1))
                )
                (List.range 0 (tiles - 1))
                |> List.concat
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- INIT


main : Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
