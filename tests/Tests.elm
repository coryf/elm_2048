module Tests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Main exposing (initBoard)
import Dict exposing (Dict)


suite : Test
suite =
    describe "Elm 2048 Tests"
        [ describe "Model"
            [ test "Board cell count is the size squared" <|
                \() ->
                    let
                        board =
                            initBoard 4
                    in
                        Expect.equal (Dict.size board.cells) (board.size ^ 2)
            ]
        ]
