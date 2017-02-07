module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Set
import GameOfLife exposing (..)


all : Test
all =
    describe "GameOfLife Test Suite"
        [ describe "Still lifes tests"
            [ test "Block" <|
                \() ->
                    let
                        w =
                            world [ cell 1 1, cell 1 2, cell 2 1, cell 2 2 ]
                    in
                        Expect.equal (evolve w 2) w
            , test "Tube" <|
                \() ->
                    let
                        w =
                            world [ cell 2 1, cell 1 2, cell 3 2, cell 2 3 ]
                    in
                        Expect.equal (evolve w 2) w
            ]
        , describe "Oscillators"
            [ test "Blinker" <|
                \() ->
                    let
                        w =
                            world [ cell 1 2, cell 2 2, cell 3 2 ]
                    in
                        Expect.equal (evolve w 2) w
            , test "Beacon" <|
                \() ->
                    let
                        w =
                            world [ cell 1 1, cell 2 1, cell 2 1, cell 1 2, cell 2 2, cell 3 3, cell 4 3, cell 3 4, cell 4 4 ]
                    in
                        Expect.equal (evolve w 2) w
            , test "Pentadecathlon" <|
                \() ->
                    let
                        w =
                            world [ cell 4 5, cell 5 5, cell 6 4, cell 6 6, cell 7 5, cell 8 5, cell 9 5, cell 10 5, cell 11 4, cell 11 6, cell 12 5, cell 13 5 ]
                    in
                        Expect.equal (evolve w 15) w
            ]
        ]
