module TapeTests exposing (..)

import Expect as E
import Fuzz exposing (oneOfValues)
import Tape exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Tape" <|
        let
            data =
                [ 1, 2, 3, 4, 5 ]

            tape1 =
                fromListOrDefault 0 data

            tape2 =
                right (right tape1)

            tape3 =
                advance tape1

            tapes =
                oneOfValues [ tape1, tape2, tape3 ]

            equalContent xs ys =
                E.equal (toList xs) ys

            reductors =
                oneOfValues
                    [ ( \x y -> String.repeat x "-" ++ y, "" )
                    , ( \x y -> String.repeat x (String.fromInt x) ++ y, "" )
                    ]

            incr x =
                x + 1
        in
        [ describe "Functions that do not change tape length" <|
            [ fuzz tapes "move RR" <|
                \xs -> equalContent (xs |> right |> right) data
            , fuzz tapes "move RL" <|
                \xs -> equalContent (xs |> right |> left) data
            , fuzz tapes "move LL" <|
                \xs -> equalContent (xs |> left |> left) data
            , describe "Tape.read" <|
                [ test "first" <|
                    \_ -> E.equal (tape1 |> read) 1
                , test "second" <|
                    \_ -> E.equal (tape1 |> right |> read) 2
                , test "last" <|
                    \_ -> E.equal (tape1 |> advance |> read) 5
                , test "back" <|
                    \_ -> E.equal (tape2 |> rewind |> read) 1
                ]
            , fuzz2 tapes reductors "Tape.foldr" <|
                \xs ( f, x0 ) -> E.equal (foldr f x0 xs) (List.foldr f x0 data)
            , fuzz tapes "Tape.foldr (sum)" <|
                \xs -> E.equal (foldr (+) 0 xs) 15
            , fuzz2 tapes reductors "Tape.foldl" <|
                \xs ( f, x0 ) -> E.equal (foldl f x0 xs) (List.foldl f x0 data)
            , fuzz tapes "Tape.foldl (sum)" <|
                \xs -> E.equal (foldl (+) 0 xs) 15
            , fuzz tapes "Tape.reduce" <|
                \xs -> E.equal (reduce (+) xs) 15
            , fuzz tapes "Tape.maximum" <|
                \xs -> E.equal (maximum xs) 5
            , fuzz tapes "Tape.minimum" <|
                \xs -> E.equal (minimum xs) 1
            , fuzz tapes "Tape.sum" <|
                \xs -> E.equal (sum xs) 15
            , fuzz tapes "Tape.product" <|
                \xs -> E.equal (product xs) 120
            , fuzz tapes "Tape.member" <|
                \xs -> E.equal (member 3 xs) True
            , fuzz tapes "Tape.length" <|
                \xs -> E.equal (length xs) 5
            , fuzz tapes "Tape.reverse" <|
                \xs -> equalContent (reverse xs) (List.reverse data)
            ]
        , describe "Mapping functions" <|
            [ fuzz tapes "Tape.map id" <|
                \xs -> E.equal (map identity xs) xs
            , fuzz tapes "Tape.indexedMap id" <|
                \xs -> E.equal (indexedMap (\_ y -> y) xs) xs
            , fuzz tapes "Tape.map (+1)" <|
                \xs -> equalContent (map incr xs) [ 2, 3, 4, 5, 6 ]
            , fuzz tapes "Tape.map (+1) |> read" <|
                \xs -> E.equal (map incr xs |> read) (incr (xs |> read))
            , test "Tape.indexedMap (+1) head" <|
                \_ -> equalContent (indexedMap (\n x -> ( n, x )) tape1) [ ( 0, 1 ), ( 1, 2 ), ( 2, 3 ), ( 3, 4 ), ( 4, 5 ) ]
            , test "Tape.indexedMap (+1)" <|
                \_ -> equalContent (indexedMap (\n x -> ( n, x )) tape2) [ ( -2, 1 ), ( -1, 2 ), ( 0, 3 ), ( 1, 4 ), ( 2, 5 ) ]
            ]
        ]
