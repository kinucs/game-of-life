module GameOfLife exposing (..)

import Set exposing (..)
import Dict exposing (..)
import Tuple exposing (..)
import Debug exposing (..)


type alias World =
    Set Cell


createWorld : List Cell -> World
createWorld cells =
    Set.fromList cells


type alias Cell =
    List Int


cell : Int -> Int -> Cell
cell x y =
    [ x, y ]


first : Cell -> Int
first cell =
    case List.head cell of
        Just x ->
            x

        Nothing ->
            -- TODO never here ? how to assert something ?
            -1


second : Cell -> Int
second cell =
    case List.tail cell of
        Just list ->
            first list

        Nothing ->
            -- TODO never here ? how to assert something ?
            -1


dimension : World -> (Cell -> Int) -> Int
dimension world axis =
    let
        max =
            Set.map axis world
                |> Set.toList
                |> List.maximum
    in
        case max of
            Just value ->
                value

            Nothing ->
                0


width : World -> Int
width world =
    dimension world first


height : World -> Int
height world =
    dimension world second


cellNeighbors : Cell -> List Cell
cellNeighbors c =
    [ cell (first c - 1) (second c - 1)
    , cell (first c) (second c - 1)
    , cell (first c + 1) (second c - 1)
    , cell (first c - 1) (second c)
    , cell (first c + 1) (second c)
    , cell (first c - 1) (second c + 1)
    , cell (first c) (second c + 1)
    , cell (first c + 1) (second c + 1)
    ]


addOneNeighborAlive : Cell -> Dict Cell Int -> Dict Cell Int
addOneNeighborAlive cell howManyNeigborsAreAlive =
    let
        nb =
            get cell howManyNeigborsAreAlive
    in
        case nb of
            Just value ->
                Dict.insert cell (value + 1) howManyNeigborsAreAlive

            Nothing ->
                Dict.insert cell 1 howManyNeigborsAreAlive


computeHowManyNeighborsAreAlive : Cell -> Dict Cell Int -> Dict Cell Int
computeHowManyNeighborsAreAlive cell nbNeightborsAlives =
    List.foldl (\c d -> addOneNeighborAlive c d) nbNeightborsAlives (cellNeighbors cell)


cellWillBeAlive : Cell -> World -> Dict Cell Int -> Bool
cellWillBeAlive cell world howManyNeigborsAreAlive =
    let
        numberNeighborsCurrentlyAlives =
            get cell howManyNeigborsAreAlive

        cellIsCurrentlyAlive =
            Set.member cell world
    in
        case cellIsCurrentlyAlive of
            True ->
                case numberNeighborsCurrentlyAlives of
                    Just number ->
                        if number < 2 then
                            False
                        else if number == 2 then
                            True
                        else if number == 3 then
                            True
                        else
                            False

                    Nothing ->
                        False

            False ->
                case numberNeighborsCurrentlyAlives of
                    Just number ->
                        number == 3

                    Nothing ->
                        False


step : World -> World
step world =
    let
        nbNeightborsAlives =
            Set.foldl (\cell howManyNeigborsAreAlive -> computeHowManyNeighborsAreAlive cell howManyNeigborsAreAlive) Dict.empty world

        cellsPossibleAlive =
            Set.fromList <| Dict.keys nbNeightborsAlives
    in
        Set.filter (\cell -> cellWillBeAlive cell world nbNeightborsAlives) cellsPossibleAlive


evolve : World -> Int -> World
evolve world steps =
    if steps == 0 then
        world
    else
        evolve (step world) (steps - 1)
