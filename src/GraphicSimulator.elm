module Main exposing (..)

import Set exposing (..)
import List exposing (..)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (Time, second)
import GameOfLife exposing (..)
import Debug exposing (..)


type alias Model =
    { time : Time
    , started : Bool
    , world : World
    }


blinker : World
blinker =
    createWorld [ cell 1 2, cell 2 2, cell 3 2 ]


pentadecathlon : World
pentadecathlon =
    createWorld [ cell 4 5, cell 5 5, cell 6 4, cell 6 6, cell 7 5, cell 8 5, cell 9 5, cell 10 5, cell 11 4, cell 11 6, cell 12 5, cell 13 5 ]


init : ( Model, Cmd Msg )
init =
    ( { time = 0, started = True, world = pentadecathlon }, Cmd.none )


type Msg
    = Tick Time
    | Switch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( Model newTime model.started (evolve model.world 1), Cmd.none )

        Switch ->
            ( { model | started = not model.started }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.started of
        True ->
            Time.every Time.second Tick

        False ->
            Sub.none


view : Model -> Html Msg
view model =
    Html.div [] [ game model ]



-- Html.div []
-- [ button [ onClick Switch ]
--     [ Html.text
--         (if model.started then
--             "Pause"
--          else
--             "Resume"
--         )
--     ]
-- , Html.div [] [ game model ]
-- ]


space : Int
space =
    50


drawCell : Cell -> Svg msg
drawCell cell =
    rect [ x ((toString <| (space * (GameOfLife.first cell)))), y ((toString <| (space * (GameOfLife.second cell)))), Svg.Attributes.width (toString space), Svg.Attributes.height (toString space), fill "##0FF000" ] []


drawCells : List Cell -> List (Svg Msg) -> List (Svg Msg)
drawCells world temp =
    if (List.isEmpty world) then
        temp
    else
        let
            head =
                List.head world
        in
            case head of
                Just cell ->
                    drawCells (List.drop 1 world) (List.append [ drawCell cell ] temp)

                Nothing ->
                    temp


drawWorld : World -> List (Svg Msg)
drawWorld world =
    drawCells (Set.toList world) []


scale : Int -> String
scale p =
    toString <| p * space


game : Model -> Html Msg
game model =
    let
        w =
            scale <| (GameOfLife.width model.world + 1)

        h =
            scale <| (GameOfLife.height model.world + 1)
    in
        -- Html.text ("w=" ++ w ++ "h=" ++ h ++ "m="  m)
        svg [ Svg.Attributes.width w, Svg.Attributes.height h, viewBox <| "0 0 " ++ w ++ " " ++ h ]
            -- [ rect
            --     [ x <| (toString (space * 1)), y <| (toString <| (space * 2)), Svg.Attributes.width (toString space), Svg.Attributes.height (toString space), fill "#FF0000" ]
            --     []
            -- , rect
            --     [ x <| (toString (space * 2)), y <| (toString <| (space * 2)), Svg.Attributes.width (toString space), Svg.Attributes.height (toString space), fill "#00FF00" ]
            --     []
            -- , rect
            --     [ x <| (toString (space * 3)), y <| (toString <| (space * 2)), Svg.Attributes.width (toString space), Svg.Attributes.height (toString space), fill "#0000FF" ]
            --     []
            -- ]
            (drawWorld model.world)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
