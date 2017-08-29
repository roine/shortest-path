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


type alias Flags =
    { width : Int, height : Int }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel flags
    , Cmd.none
    )


initialModel : Flags -> Model
initialModel flags =
    { points = Dict.empty, isMouseDown = False }



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []



-- VIEW
--


view : Model -> Html Msg
view model =
    Html.text ""



-- INIT


main =
    programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
