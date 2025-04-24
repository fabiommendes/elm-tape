module Doc.TapeSpec exposing (spec)

import Test
import Expect
import Tape exposing(..)


spec : Test.Test
spec =
    Test.describe "Tape" <|
        [ Test.describe "#create" <|
            [ Test.test "Example: 1 -- `create [ \"a\", \"b\" ] \"c\" [ \"d\", \"e\" ] ...`" <|
                \() ->
                    Expect.equal
                        (
                            create [ "a", "b" ] "c" [ "d", "e" ]
                                |> show identity
                        )
                        (
                            "[ a, b, => c, d, e ]"
                        )
            ]
        , Test.describe "#fromList" <|
            [ Test.test "Example: 1 -- `(fromList [ 1, 2, 3 ] |> Maybe.map to...`" <|
                \() ->
                    Expect.equal
                        (
                            (fromList [ 1, 2, 3 ]
                                |> Maybe.map toList)
                        )
                        (
                            Just [ 1, 2, 3 ]
                        )
            ]
        , Test.describe "#fromListOrDefault" <|
            [ Test.test "Example: 1 -- `fromListOrDefault 0 [ 1, 2, 3 ] |> to...`" <|
                \() ->
                    Expect.equal
                        (
                            fromListOrDefault 0 [ 1, 2, 3 ]
                                |> toList
                        )
                        (
                            [ 1, 2, 3 ]
                        )
            ]
        , Test.describe "#repeat" <|
            [ Test.test "Example: 1 -- `repeat 2 5 \"a\" |> show identity --> \"...`" <|
                \() ->
                    Expect.equal
                        (
                            repeat 2 5 "a"
                                |> show identity
                        )
                        (
                            "[ a, a, => a, a, a ]"
                        )
            ]
        , Test.describe "#range" <|
            [ Test.test "Example: 1 -- `range 0 5 |> toList --> [ 0, 1, 2, 3,...`" <|
                \() ->
                    Expect.equal
                        (
                            range 0 5
                                |> toList
                        )
                        (
                            [ 0, 1, 2, 3, 4, 5 ]
                        )
            ]
        , Test.describe "#read" <|
            [ Test.test "Example: 1 -- `create [1, 2] 3 [4, 5] |> read --> 3`" <|
                \() ->
                    Expect.equal
                        (
                            create [1, 2] 3 [4, 5]
                                |> read
                        )
                        (
                            3
                        )
            ]
        , Test.describe "#write" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> w...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> write "changed"
                                |> show identity
                        )
                        (
                            "[ a, b, => changed, d, e ]"
                        )
            ]
        , Test.describe "#pushRight" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> p...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> pushRight "changed"
                                |> show identity
                        )
                        (
                            "[ a, b, c, => changed, d, e ]"
                        )
            ]
        , Test.describe "#pushLeft" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> p...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> pushLeft "changed"
                                |> show identity
                        )
                        (
                            "[ a, b, => changed, c, d, e ]"
                        )
            ]
        , Test.describe "#undo" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\", \"c\"] \"d\" [\"e\"] |> u...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b", "c"] "d" ["e"]
                                |> undo
                                |> show identity
                        )
                        (
                            "[ a, b, => c, d, e ]"
                        )
            ]
        , Test.describe "#redo" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\", \"c\"] \"d\" [\"e\"] |> r...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b", "c"] "d" ["e"]
                                |> redo
                                |> show identity
                        )
                        (
                            "[ a, b, c, d, => e ]"
                        )
            ]
        , Test.describe "#left" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\", \"c\"] \"d\" [\"e\"] |> l...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b", "c"] "d" ["e"]
                                |> left 2
                                |> show identity
                        )
                        (
                            "[ a, => b, c, d, e ]"
                        )
            ]
        , Test.describe "#right" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\", \"c\"] \"d\" [\"e\"] |> r...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b", "c"] "d" ["e"]
                                |> right 2
                                |> show identity
                        )
                        (
                            "[ a, b, c, d, => e ]"
                        )
            ]
        , Test.describe "#leftWith" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> l...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> leftWith (Cycle 3)
                                |> show identity
                        )
                        (
                            "[ a, b, c, d, => e ]"
                        )
            , Test.test "Example: 2 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> l...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                 |> leftWith (Default ("?", 4))
                                 |> show identity
                        )
                        (
                            "[ => ?, ?, a, b, c, d, e ]"
                        )
            , Test.test "Example: 3 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> l...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                 |> leftWith (Condition ((/=) "b"))
                                 |> show identity
                        )
                        (
                            "[ a, => b, c, d, e ]"
                        )
            , Test.test "Example: 4 -- `create [\"a\", \"b\"] \"cc\" [\"d\", \"e\"] |> ...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "cc" ["d", "e"]
                                |> leftWith (Carry 2)
                                |> show identity
                        )
                        (
                            "[ => cc, a, b, d, e ]"
                        )
            ]
        , Test.describe "#rightWith" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> r...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> rightWith (Cycle 3)
                                |> show identity
                        )
                        (
                            "[ => a, b, c, d, e ]"
                        )
            , Test.test "Example: 2 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> r...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                    |> rightWith (Default ("?", 4))
                                    |> show identity
                        )
                        (
                            "[ a, b, c, d, e, ?, => ? ]"
                        )
            , Test.test "Example: 3 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> r...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                    |> rightWith (Condition ((/=) "b"))
                                    |> show identity
                        )
                        (
                            "[ a, b, c, d, => e ]"
                        )
            , Test.test "Example: 4 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> r...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> rightWith (Carry 2)
                                |> show identity
                        )
                        (
                            "[ a, b, d, e, => c ]"
                        )
            ]
        , Test.describe "#tryLeft" <|
            [ Test.test "Example: 1 -- `create [\"a\"] \"b\" [\"c\", \"d\", \"e\"] |> t...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a"] "b" ["c", "d", "e"]
                                |> tryLeft 2
                        )
                        (
                            Nothing
                        )
            ]
        , Test.describe "#tryRight" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\"] \"c\" [\"d\"] |> tryRig...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d"]
                                |> tryRight 2
                        )
                        (
                            Nothing
                        )
            ]
        , Test.describe "#rewind" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> r...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> rewind
                                |> show identity
                        )
                        (
                            "[ => a, b, c, d, e ]"
                        )
            ]
        , Test.describe "#advance" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> a...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> advance
                                |> show identity
                        )
                        (
                            "[ a, b, c, d, => e ]"
                        )
            ]
        , Test.describe "#map" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> m...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> map String.toUpper
                                |> show identity
                        )
                        (
                            "[ A, B, => C, D, E ]"
                        )
            ]
        , Test.describe "#map2" <|
            [ Test.test "Example: 1 -- `(map2 (++) (create [\"a\", \"b\"] \"c\" [\"d...`" <|
                \() ->
                    Expect.equal
                        (
                            (map2 (++)
                                (create ["a", "b"] "c" ["d", "e"])
                                (repeat 1 5 "x")
                            )
                                |> show identity
                        )
                        (
                            "[ bx, => cx, dx, ex ]"
                        )
            ]
        , Test.describe "#update" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> u...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> update Right String.toUpper
                                |> show identity
                        )
                        (
                            "[ a, b, => C, D, E ]"
                        )
            ]
        , Test.describe "#select" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> s...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> select All
                        )
                        (
                            [ "a", "b", "c", "d", "e" ]
                        )
            , Test.test "Example: 2 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> s...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> select Left
                        )
                        (
                            [ "a", "b", "c" ]
                        )
            , Test.test "Example: 3 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> s...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> select Backwards
                        )
                        (
                            [ "c", "b", "a" ]
                        )
            , Test.test "Example: 4 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> s...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> select Head
                        )
                        (
                            [ "c" ]
                        )
            , Test.test "Example: 5 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> s...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> select Tails
                        )
                        (
                            [ "a", "b", "d", "e" ]
                        )
            ]
        , Test.describe "#intersperse" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> i...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> intersperse "x"
                                |> show identity
                        )
                        (
                            "[ a, x, b, x, => c, x, d, x, e ]"
                        )
            ]
        , Test.describe "#sort" <|
            [ Test.test "Example: 1 -- `create [\"d\"] \"c\" [\"a\", \"e\", \"b\"] |> s...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["d"] "c" ["a", "e", "b"]
                                |> sort
                                |> show identity
                        )
                        (
                            "[ a, b, => c, d, e ]"
                        )
            ]
        , Test.describe "#sortBy" <|
            [ Test.test "Example: 1 -- `create [\"d\"] \"cc\" [\"a\", \"eee\", \"bb\"] ...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["d"] "cc" ["a", "eee", "bb"]
                                |> sortBy String.length
                                |> show identity
                        )
                        (
                            "[ d, a, => cc, bb, eee ]"
                        )
            ]
        , Test.describe "#unzip" <|
            [ Test.test "Example: 1 -- `create [(\"a\", 1), (\"b\", 2)] (\"c\", 3) ...`" <|
                \() ->
                    Expect.equal
                        (
                            create [("a", 1), ("b", 2)] ("c", 3) [("d", 4), ("e", 5)]
                                |> unzip
                                |> Tuple.mapBoth (show identity) (show String.fromInt)
                        )
                        (
                            ( "[ a, b, => c, d, e ]", "[ 1, 2, => 3, 4, 5 ]" )
                        )
            ]
        , Test.describe "#indexedMap" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> i...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> indexedMap (\i x -> String.fromInt i ++ x)
                                |> show identity
                        )
                        (
                            "[ -2a, -1b, => 0c, 1d, 2e ]"
                        )
            ]
        , Test.describe "#positionalMap" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> p...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> positionalMap (\i x -> String.fromInt i ++ x)
                                |> show identity
                        )
                        (
                            "[ 0a, 1b, => 2c, 3d, 4e ]"
                        )
            ]
        , Test.describe "#foldl" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> f...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> foldl (++) "$"
                        )
                        (
                            "edcba$"
                        )
            ]
        , Test.describe "#foldr" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> f...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> foldr (++) "$"
                        )
                        (
                            "abcde$"
                        )
            ]
        , Test.describe "#reduce" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> r...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> reduce (++)
                        )
                        (
                            "abcde"
                        )
            , Test.test "Example: 2 -- `create [] \"c\" [\"d\", \"e\"] |> reduce (+...`" <|
                \() ->
                    Expect.equal
                        (
                            create [] "c" ["d", "e"]
                                |> reduce (++)
                        )
                        (
                            "cde"
                        )
            ]
        , Test.describe "#toList" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> t...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> toList
                        )
                        (
                            ["a", "b", "c", "d", "e"]
                        )
            ]
        , Test.describe "#length" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> l...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> length
                        )
                        (
                            5
                        )
            ]
        , Test.describe "#position" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> p...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> position
                        )
                        (
                            2
                        )
            ]
        , Test.describe "#reverse" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> r...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> reverse
                                |> show identity
                        )
                        (
                            "[ e, d, => c, b, a ]"
                        )
            ]
        , Test.describe "#member" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> m...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> member "b"
                        )
                        (
                            True
                        )
            ]
        , Test.describe "#all" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> a...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> all (\x -> String.length x < 2)
                        )
                        (
                            True
                        )
            ]
        , Test.describe "#any" <|
            [ Test.test "Example: 1 -- `create [\"a\", \"b\"] \"c\" [\"d\", \"e\"] |> a...`" <|
                \() ->
                    Expect.equal
                        (
                            create ["a", "b"] "c" ["d", "e"]
                                |> any (\x -> x == "e")
                        )
                        (
                            True
                        )
            ]
    ]