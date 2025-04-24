module Tape exposing
    ( Tape
    , create, fromList, fromListOrDefault, fromParts, singleton, repeat, range
    , read, write, advance, rewind, left, right, undo, redo
    , How(..), leftWith, rightWith, tryLeft, tryRight, pushLeft, pushRight
    , map, map2, map3, map4, map5, indexedMap, positionalMap
    , reverse, sort, sortBy, sortWith, intersperse
    , Direction(..), select, update, take, filter
    , unzip, parts
    , toList, show
    , foldr, foldl, reduce, maximum, minimum, sum, product, all, any
    , length, position, member, isEmptyLeft, isEmptyRight, elemIndex
    )

{-| A linear structure with a cursor pointing to some specific element. Tapes cannot be empty.

@docs Tape


## Create

@docs create, fromList, fromListOrDefault, fromParts, singleton, repeat, range


## Move operations

@docs read, write, advance, rewind, left, right, undo, redo


## Advanced move operators

@docs How, leftWith, rightWith, tryLeft, tryRight, pushLeft, pushRight


## Mapping

@docs map, map2, map3, map4, map5, indexedMap, positionalMap


## Transforms

@docs reverse, sort, sortBy, sortWith, intersperse


## Sub-tapes

@docs Direction, select, update, take, filter


## Deconstruction

@docs unzip, parts


## Conversions

@docs toList, show


## Summarizing and folding

@docs foldr, foldl, reduce, maximum, minimum, sum, product, all, any


## Properties

@docs length, position, member, isEmptyLeft, isEmptyRight, elemIndex

-}

import List.Extra as List


{-| Base iterator type
-}
type Tape a
    = Tape Int (List a) a (List a)



--------------------------------------------------------------------------------
-- CREATE
--------------------------------------------------------------------------------


{-| Create a tape from the left, head, and right parts.

The tape is conceptually equivalent to the list `left ++ [head] ++ right`
and has a cursor that points to the tape head.

    create [ "a", "b" ] "c" [ "d", "e" ]
        |> show identity
        --> "[ a, b, => c, d, e ]"

-}
create : List a -> a -> List a -> Tape a
create xs y zs =
    Tape (List.length xs) (List.reverse xs) y zs


{-| Try to create tape from List. Empty lists return Nothing.

    (fromList [ 1, 2, 3 ]
        |> Maybe.map toList)
        --> Just [ 1, 2, 3 ]

-}
fromList : List a -> Maybe (Tape a)
fromList xs =
    case xs of
        x :: ys ->
            Just (Tape 0 [] x ys)

        _ ->
            Nothing


{-| Construct tape from left and right tails.

    fromParts (tail Left tape) (head tape) (tail Right tape) ==> tape

-}
fromParts : List a -> a -> List a -> Tape a
fromParts xs y zs =
    Tape (List.length xs) xs y zs


{-| Create a tape from List or a singleton tape with the given default value.

    fromListOrDefault 0 [ 1, 2, 3 ]
        |> toList
        --> [ 1, 2, 3 ]

-}
fromListOrDefault : a -> List a -> Tape a
fromListOrDefault x xs =
    case xs of
        y :: ys ->
            Tape 0 [] y ys

        _ ->
            singleton x


{-| Create a tape with only one element
-}
singleton : a -> Tape a
singleton a =
    Tape 0 [] a []


{-| Create a tape with n copies of a value and tries to place m of them
before the head.

`repeat 0` behaves similarly to List.repeat, creating a tape with n elements
pointing to the first element.

Empty negative n are ignored, returning a tape with a single element.

    repeat 2 5 "a"
        |> show identity
        --> "[ a, a, => a, a, a ]"

-}
repeat : Int -> Int -> a -> Tape a
repeat m n x =
    if m >= n then
        Tape n (List.repeat (n - 1) x) x []

    else
        Tape m (List.repeat m x) x (List.repeat (n - m - 1) x)


{-| Create a tape of numbers, every element increasing by one.

The arguments are the lowest and highest numbers that should be on the tape.

    range 0 5
        |> toList
        --> [ 0, 1, 2, 3, 4, 5 ]

-}
range : Int -> Int -> Tape Int
range a b =
    if b > a then
        Tape 0 [] a (List.range (a + 1) b)

    else
        singleton a



--------------------------------------------------------------------------------
-- TAPE OPERATIONS
--------------------------------------------------------------------------------


{-| Read element on the current position.

    create [1, 2] 3 [4, 5]
        |> read
        --> 3

-}
read : Tape a -> a
read (Tape _ _ x _) =
    x


{-| Write element to the tape's current position.

    create ["a", "b"] "c" ["d", "e"]
        |> write "changed"
        |> show identity
        --> "[ a, b, => changed, d, e ]"

-}
write : a -> Tape a -> Tape a
write y (Tape n xs _ zs) =
    Tape n xs y zs


{-| Push element to the right of the current head and move the cursor to it.

    create ["a", "b"] "c" ["d", "e"]
        |> pushRight "changed"
        |> show identity
        --> "[ a, b, c, => changed, d, e ]"

-}
pushRight : a -> Tape a -> Tape a
pushRight new (Tape n xs y zs) =
    Tape (n + 1) (y :: xs) new zs


{-| Push element to the left of the current head and move the cursor to it.

    create ["a", "b"] "c" ["d", "e"]
        |> pushLeft "changed"
        |> show identity
        --> "[ a, b, => changed, c, d, e ]"

-}
pushLeft : a -> Tape a -> Tape a
pushLeft new (Tape n xs y zs) =
    Tape n xs new (y :: zs)


{-| An alias to `left 1` that is more idiomatic when using the tape as a history container.

    create ["a", "b", "c"] "d" ["e"]
        |> undo
        |> show identity
            --> "[ a, b, => c, d, e ]"

-}
undo : Tape a -> Tape a
undo ((Tape n xs y zs) as tape) =
    case xs of
        [] ->
            tape

        w :: ws ->
            Tape (n - 1) ws w (y :: zs)


{-| An alias to `right 1` that is more idiomatic when using the tape as a history container.

    create ["a", "b", "c"] "d" ["e"]
        |> redo
        |> show identity
            --> "[ a, b, c, d, => e ]"

-}
redo : Tape a -> Tape a
redo ((Tape n xs y zs) as tape) =
    case zs of
        [] ->
            tape

        w :: ws ->
            Tape (n + 1) (y :: xs) w ws


{-| Move at most n positions to the left.

Negative steps move to the right.

    create ["a", "b", "c"] "d" ["e"]
        |> left 2
        |> show identity
        --> "[ a, => b, c, d, e ]"

-}
left : Int -> Tape a -> Tape a
left n ((Tape m _ _ _) as tape) =
    if n == 0 || m == 0 then
        tape

    else if n > 0 then
        left (n - 1) (undo tape)

    else
        left (n + 1) (redo tape)


{-| Move at most n positions to the right.

Negative values move to the left.

    create ["a", "b", "c"] "d" ["e"]
        |> right 2
        |> show identity
        --> "[ a, b, c, d, => e ]"

-}
right : Int -> Tape a -> Tape a
right n =
    left -n


{-| Declare how to move in the tape.
-}
type How a
    = Cycle Int
    | Default ( a, Int )
    | Condition (a -> Bool)
    | Carry Int


{-| Move head to the left according to strategy

    create ["a", "b"] "c" ["d", "e"]
        |> leftWith (Cycle 3)
        |> show identity
        --> "[ a, b, c, d, => e ]"

    create ["a", "b"] "c" ["d", "e"]
         |> leftWith (Default ("?", 4))
         |> show identity
         --> "[ => ?, ?, a, b, c, d, e ]"

    create ["a", "b"] "c" ["d", "e"]
         |> leftWith (Condition ((/=) "b"))
         |> show identity
         --> "[ a, => b, c, d, e ]"

    create ["a", "b"] "cc" ["d", "e"]
        |> leftWith (Carry 2)
        |> show identity
        --> "[ => cc, a, b, d, e ]"

-}
leftWith : How a -> Tape a -> Tape a
leftWith how tape =
    case how of
        Cycle n ->
            leftCycle n tape

        Default ( a, n ) ->
            leftDefault a n tape

        Condition pred ->
            leftWhile pred tape

        Carry n ->
            leftCarry n tape


{-| Move head to the right according to strategy

    create ["a", "b"] "c" ["d", "e"]
        |> rightWith (Cycle 3)
        |> show identity
        --> "[ => a, b, c, d, e ]"

    create ["a", "b"] "c" ["d", "e"]
            |> rightWith (Default ("?", 4))
            |> show identity
            --> "[ a, b, c, d, e, ?, => ? ]"

    create ["a", "b"] "c" ["d", "e"]
            |> rightWith (Condition ((/=) "b"))
            |> show identity
            --> "[ a, b, c, d, => e ]"

    create ["a", "b"] "c" ["d", "e"]
        |> rightWith (Carry 2)
        |> show identity
        --> "[ a, b, d, e, => c ]"

-}
rightWith : How a -> Tape a -> Tape a
rightWith how tape =
    case how of
        Cycle n ->
            leftCycle -n tape

        Default ( a, n ) ->
            leftDefault a -n tape

        Condition pred ->
            rightWhile pred tape

        Carry n ->
            leftCarry -n tape


leftCycle : Int -> Tape a -> Tape a
leftCycle n ((Tape m _ _ zs) as tape) =
    if n == 0 then
        tape

    else if n > 0 && m > 0 then
        leftCycle (n - 1) (undo tape)

    else if n > 0 && m <= 0 then
        leftCycle (n - 1) (advance tape)

    else if zs == [] then
        leftCycle (n + 1) (rewind tape)

    else
        leftCycle (n + 1) (redo tape)


leftWhile : (a -> Bool) -> Tape a -> Tape a
leftWhile pred ((Tape m _ y _) as tape) =
    if m > 0 && pred y then
        leftWhile pred (undo tape)

    else
        tape


rightWhile : (a -> Bool) -> Tape a -> Tape a
rightWhile pred ((Tape _ _ y zs) as tape) =
    if not (List.isEmpty zs) && pred y then
        rightWhile pred (redo tape)

    else
        tape


leftDefault : a -> Int -> Tape a -> Tape a
leftDefault default n ((Tape m xs y zs) as tape) =
    if n == 0 then
        tape

    else if n > 0 then
        case xs of
            [] ->
                leftDefault default (n - 1) (Tape 0 xs default (y :: zs))

            w :: ws ->
                leftDefault default (n - 1) (Tape (m - 1) ws w (y :: zs))

    else
        case zs of
            [] ->
                leftDefault default (n + 1) (Tape (m + 1) (y :: xs) default zs)

            w :: ws ->
                leftDefault default (n + 1) (Tape (m + 1) (y :: xs) w ws)


leftCarry : Int -> Tape a -> Tape a
leftCarry n ((Tape m xs y zs) as tape) =
    if n == 0 then
        tape

    else if n > 0 then
        case xs of
            [] ->
                tape

            w :: ws ->
                leftCarry (n - 1) (Tape (m - 1) ws y (w :: zs))

    else
        case zs of
            [] ->
                tape

            w :: ws ->
                leftCarry (n + 1) (Tape (m + 1) (w :: xs) y ws)


{-| Try to move to the left.

Return Nothing if the tape would overflow on the leftmost position.

    create ["a"] "b" ["c", "d", "e"]
        |> tryLeft 2
        --> Nothing

-}
tryLeft : Int -> Tape a -> Maybe (Tape a)
tryLeft n ((Tape m xs y zs) as tape) =
    if n == 0 then
        Just tape

    else if n > m then
        Nothing

    else if n > 0 then
        Just (left n tape)

    else if n == 0 then
        Just tape

    else
        case zs of
            [] ->
                Nothing

            w :: ws ->
                tryLeft (n + 1) (Tape (m + 1) (y :: xs) w ws)


{-| Try to move to the right.

Return Nothing if the tape would overflow on the rightmost position.

    create ["a", "b"] "c" ["d"]
        |> tryRight 2
        --> Nothing

-}
tryRight : Int -> Tape a -> Maybe (Tape a)
tryRight n =
    tryLeft -n


{-| Rewind tape so the head points to the first element

    create ["a", "b"] "c" ["d", "e"]
        |> rewind
        |> show identity
        --> "[ => a, b, c, d, e ]"

-}
rewind : Tape a -> Tape a
rewind ((Tape _ xs y zs) as tape) =
    case xs of
        [] ->
            tape

        w :: ws ->
            rewind (Tape 0 ws w (y :: zs))


{-| Advance tape to the last element

    create ["a", "b"] "c" ["d", "e"]
        |> advance
        |> show identity
        --> "[ a, b, c, d, => e ]"

-}
advance : Tape a -> Tape a
advance ((Tape n xs y zs) as tape) =
    case zs of
        [] ->
            tape

        w :: ws ->
            advance (Tape (n + 1) (y :: xs) w ws)



--------------------------------------------------------------------------------
-- TRANSFORM
--------------------------------------------------------------------------------


{-| Apply a function to every element of a tape.

    create ["a", "b"] "c" ["d", "e"]
        |> map String.toUpper
        |> show identity
        --> "[ A, B, => C, D, E ]"

-}
map : (a -> b) -> Tape a -> Tape b
map f (Tape n xs y zs) =
    Tape n (List.map f xs) (f y) (List.map f zs)


{-| Combine two tapes with function.

It keeps the smallest left and right tails of each tape.

    (map2 (++)
        (create ["a", "b"] "c" ["d", "e"])
        (repeat 1 5 "x")
    )
        |> show identity
        --> "[ bx, => cx, dx, ex ]"

-}
map2 : (a -> b -> result) -> Tape a -> Tape b -> Tape result
map2 f (Tape n1 xs1 y1 zs1) (Tape n2 xs2 y2 zs2) =
    Tape (min n1 n2) (List.map2 f xs1 xs2) (f y1 y2) (List.map2 f zs1 zs2)


{-| Generalize map for 3 tapes
-}
map3 : (a -> b -> c -> result) -> Tape a -> Tape b -> Tape c -> Tape result
map3 f (Tape n1 xs1 y1 zs1) (Tape n2 xs2 y2 zs2) (Tape n3 xs3 y3 zs3) =
    Tape (min n1 (min n2 n3)) (List.map3 f xs1 xs2 xs3) (f y1 y2 y3) (List.map3 f zs1 zs2 zs3)


{-| Generalize map for 4 tapes
-}
map4 : (a -> b -> c -> d -> result) -> Tape a -> Tape b -> Tape c -> Tape d -> Tape result
map4 f (Tape n1 xs1 y1 zs1) (Tape n2 xs2 y2 zs2) (Tape n3 xs3 y3 zs3) (Tape n4 xs4 y4 zs4) =
    Tape (min n1 (min n2 (min n3 n4))) (List.map4 f xs1 xs2 xs3 xs4) (f y1 y2 y3 y4) (List.map4 f zs1 zs2 zs3 zs4)


{-| Generalize map for 5 tapes
-}
map5 : (a -> b -> c -> d -> e -> result) -> Tape a -> Tape b -> Tape c -> Tape d -> Tape e -> Tape result
map5 f (Tape n1 xs1 y1 zs1) (Tape n2 xs2 y2 zs2) (Tape n3 xs3 y3 zs3) (Tape n4 xs4 y4 zs4) (Tape n5 xs5 y5 zs5) =
    Tape (min n1 (min n2 (min n3 (min n4 n5)))) (List.map5 f xs1 xs2 xs3 xs4 xs5) (f y1 y2 y3 y4 y5) (List.map5 f zs1 zs2 zs3 zs4 zs5)


{-| The direction enum selects parts of the tape that shall be extracted/modified.

Each function might interpret each direction slightly differently. Check the
documentation for more details.

-}
type Direction
    = Left
    | Right
    | LeftTail
    | Backwards
    | BackwardsTail
    | RightTail
    | Head
    | Tails
    | InsideOut
    | All


{-| A partial map on the tape, preserving the types of elements.

We can specify a direction to update from the head

    create ["a", "b"] "c" ["d", "e"]
        |> update Right String.toUpper
        |> show identity
        --> "[ a, b, => C, D, E ]"

The direction can be one of the following:

    - `Left` - only the left part is modified, including the head.
    - `Right` - only the right part is modified, including the head.
    - `LeftTail` - only the left part is modified, excluding the head.
    - `RightTail` - only the right part is modified, excluding the head.
    - `Backwards` - same as Left.
    - `BackwardsTail` - same as LeftTail.
    - `Head` - only the head is modified.
    - `Tails` - modify everything exept the head.
    - `InsideOut` - like All.
    - `All` - all elements are modified.

-}
update : Direction -> (a -> a) -> Tape a -> Tape a
update direction f ((Tape n xs y zs) as tape) =
    case direction of
        All ->
            Tape n (List.map f xs) (f y) (List.map f zs)

        InsideOut ->
            Tape n (List.map f xs) (f y) (List.map f zs)

        Left ->
            Tape n (List.map f xs) (f y) zs

        Backwards ->
            update Left f tape

        LeftTail ->
            Tape n (List.map f xs) (f y) zs

        BackwardsTail ->
            update LeftTail f tape

        Right ->
            Tape n xs (f y) (List.map f zs)

        RightTail ->
            Tape n xs y (List.map f zs)

        Head ->
            Tape n xs (f y) zs

        Tails ->
            Tape n (List.map f zs) y (List.map f zs)


{-| Select parts of a tape and return it as a list

    create ["a", "b"] "c" ["d", "e"]
        |> select All
        --> [ "a", "b", "c", "d", "e" ]

    create ["a", "b"] "c" ["d", "e"]
        |> select Left
        --> [ "a", "b", "c" ]

    create ["a", "b"] "c" ["d", "e"]
        |> select Backwards
        --> [ "c", "b", "a" ]

    create ["a", "b"] "c" ["d", "e"]
        |> select Head
        --> [ "c" ]

    create ["a", "b"] "c" ["d", "e"]
        |> select Tails
        --> [ "a", "b", "d", "e" ]

-}
select : Direction -> Tape a -> List a
select direction (Tape _ xs y zs) =
    case direction of
        All ->
            List.reverse xs ++ (y :: zs)

        InsideOut ->
            xs ++ (y :: zs)

        Left ->
            List.reverse (y :: xs)

        Backwards ->
            y :: xs

        LeftTail ->
            List.reverse xs

        BackwardsTail ->
            xs

        Right ->
            y :: zs

        RightTail ->
            zs

        Head ->
            [ y ]

        Tails ->
            List.reverse xs ++ zs


{-| Take elements from one specific direction on the tape
-}
take : Direction -> Int -> Tape a -> Tape a
take direction n ((Tape m xs y zs) as tape) =
    case direction of
        Tails ->
            Tape (min n m) (List.take n xs) y (List.take n zs)

        InsideOut ->
            Tape (min (n - 1) m) (List.take (n - 1) xs) y (List.take (n - 1) zs)

        All ->
            take InsideOut n tape

        Left ->
            Tape (min (n - 1) m) (List.take (n - 1) xs) y zs

        Backwards ->
            take Left n tape

        LeftTail ->
            Tape (min n m) (List.take n xs) y zs

        BackwardsTail ->
            take LeftTail n tape

        Right ->
            Tape m xs y (List.take (n - 1) zs)

        RightTail ->
            Tape m xs y (List.take n zs)

        Head ->
            Tape 0 [] y []


{-| Filter the given selection of tape with predicate.

Filter never removes the head, which makes `filter Head` a no-op and
`filter DirectoinTail` and `filter Direction` equivalent.

If you want to filter the head, convert to a list before

-}
filter : Direction -> (a -> Bool) -> Tape a -> Tape a
filter direction pred ((Tape n xs y zs) as tape) =
    case direction of
        All ->
            Tape n (List.filter pred xs) y (List.filter pred zs)

        InsideOut ->
            filter All pred tape

        Tails ->
            filter All pred tape

        Left ->
            Tape n (List.filter pred xs) y zs

        Backwards ->
            filter Left pred tape

        LeftTail ->
            filter Left pred tape

        BackwardsTail ->
            filter Left pred tape

        Right ->
            Tape n xs y (List.filter pred zs)

        RightTail ->
            filter Right pred tape

        Head ->
            tape


{-| Places the given value between all members of the given tape.

    create ["a", "b"] "c" ["d", "e"]
        |> intersperse "x"
        |> show identity
        --> "[ a, x, b, x, => c, x, d, x, e ]"

-}
intersperse : a -> Tape a -> Tape a
intersperse x (Tape n xs y zs) =
    Tape (max 0 (2 * n - 1)) (x :: List.intersperse x xs) y (x :: List.intersperse x zs)


{-| Sort values from lowest to highest

Head moves to keep track of current element.

    create ["d"] "c" ["a", "e", "b"]
        |> sort
        |> show identity
        --> "[ a, b, => c, d, e ]"

-}
sort : Tape comparable -> Tape comparable
sort =
    sortBy identity


{-| Sort values by a derived property.

Head moves to keep track of current element.

    create ["d"] "cc" ["a", "eee", "bb"]
        |> sortBy String.length
        |> show identity
        --> "[ d, a, => cc, bb, eee ]"

-}
sortBy : (a -> comparable) -> Tape a -> Tape a
sortBy f (Tape _ xs y zs) =
    let
        keyed =
            ( True, y ) :: (List.map (Tuple.pair False) xs ++ List.map (Tuple.pair False) zs)
    in
    keyed
        |> List.sortBy (\( _, x ) -> f x)
        |> fromListOrDefault ( False, y )
        |> rightWhile (\( key, _ ) -> not key)
        |> map Tuple.second


{-| Sort values with a custom comparison function.

Head moves to keep track of current element.

-}
sortWith : (a -> a -> Order) -> Tape a -> Tape a
sortWith f (Tape _ xs h zs) =
    let
        keyed =
            ( True, h ) :: (List.map (Tuple.pair False) xs ++ List.map (Tuple.pair False) zs)
    in
    keyed
        |> List.sortWith (\( _, x ) ( _, y ) -> f x y)
        |> fromListOrDefault ( False, h )
        |> rightWhile (\( key, _ ) -> not key)
        |> map Tuple.second


{-| Decompose a tape of tuples into a tuple of tapes.

    create [("a", 1), ("b", 2)] ("c", 3) [("d", 4), ("e", 5)]
        |> unzip
        |> Tuple.mapBoth (show identity) (show String.fromInt)
        --> ( "[ a, b, => c, d, e ]", "[ 1, 2, => 3, 4, 5 ]" )

-}
unzip : Tape ( a, b ) -> ( Tape a, Tape b )
unzip (Tape n xs ( a, b ) zs) =
    let
        ( xas, xbs ) =
            List.unzip xs

        ( zas, zbs ) =
            List.unzip zs
    in
    ( Tape n xas a zas, Tape n xbs b zbs )


{-| Same as map but the function is also applied to the index of each element.

The head has an index of zero and the left hand side of the tape is indexed with
negative numbers.

    create ["a", "b"] "c" ["d", "e"]
        |> indexedMap (\i x -> String.fromInt i ++ x)
        |> show identity
        --> "[ -2a, -1b, => 0c, 1d, 2e ]"

-}
indexedMap : (Int -> a -> b) -> Tape a -> Tape b
indexedMap f (Tape m xs y zs) =
    let
        f1 n x =
            f -(n + 1) x

        f2 n x =
            f (n + 1) x
    in
    Tape m (List.indexedMap f1 xs) (f 0 y) (List.indexedMap f2 zs)


{-| Similar to indexedMap, but uses the position of each element, which is
equivalent to the index of a rewinded tape.

    create ["a", "b"] "c" ["d", "e"]
        |> positionalMap (\i x -> String.fromInt i ++ x)
        |> show identity
        --> "[ 0a, 1b, => 2c, 3d, 4e ]"

-}
positionalMap : (Int -> a -> b) -> Tape a -> Tape b
positionalMap f (Tape m xs y zs) =
    let
        f1 n x =
            f (m - n - 1) x

        f2 n x =
            f (m + n + 1) x
    in
    Tape m (List.indexedMap f1 xs) (f m y) (List.indexedMap f2 zs)


{-| Reduce a tape from the left.

It has the same effect as reducing the equivalent list.

    create ["a", "b"] "c" ["d", "e"]
        |> foldl (++) "$"
        --> "edcba$"

-}
foldl : (a -> b -> b) -> b -> Tape a -> b
foldl f acc (Tape _ xs y zs) =
    List.foldl f (List.foldr f acc xs) (y :: zs)


{-| Reduce a tape from the right.

It has the same effect as reducing the equivalent list.

    create ["a", "b"] "c" ["d", "e"]
        |> foldr (++) "$"
        --> "abcde$"

-}
foldr : (a -> b -> b) -> b -> Tape a -> b
foldr f acc (Tape _ xs y zs) =
    List.foldl f (List.foldr f acc zs) (y :: xs)


{-| Reduce tape applying binary operator.

Similar to a left fold, but it does not require an initial accumulator.

    create ["a", "b"] "c" ["d", "e"]
        |> reduce (++)
        --> "abcde"

    create [] "c" ["d", "e"]
        |> reduce (++)
        --> "cde"

-}
reduce : (a -> a -> a) -> Tape a -> a
reduce op (Tape _ xs_ y zs) =
    let
        start =
            case List.reverse xs_ of
                [] ->
                    y

                x :: xs ->
                    op (List.foldl (\a b -> op b a) x xs) y
    in
    List.foldl (\a b -> op b a) start zs


{-| Converts tape to list

    create ["a", "b"] "c" ["d", "e"]
        |> toList
        --> ["a", "b", "c", "d", "e"]

-}
toList : Tape a -> List a
toList (Tape _ xs y zs) =
    List.reverse xs ++ (y :: zs)



--------------------------------------------------------------------------------
-- UTILITIES
--------------------------------------------------------------------------------


{-| Determine the length of a tape.

    create ["a", "b"] "c" ["d", "e"]
        |> length
        --> 5

-}
length : Tape a -> Int
length (Tape n _ _ ys) =
    n + List.length ys + 1


{-| Determine the position of head.

Position corresponds to the number of steps we must move
right from a rewinded tape.

    create ["a", "b"] "c" ["d", "e"]
        |> position
        --> 2

-}
position : Tape a -> Int
position (Tape n _ _ _) =
    n


{-| Flip a tape around the head.

    create ["a", "b"] "c" ["d", "e"]
        |> reverse
        |> show identity
        --> "[ e, d, => c, b, a ]"

-}
reverse : Tape a -> Tape a
reverse (Tape _ xs y zs) =
    Tape (List.length zs) zs y xs


{-| Figure out whether a tape contains a value.

    create ["a", "b"] "c" ["d", "e"]
        |> member "b"
        --> True

-}
member : a -> Tape a -> Bool
member a (Tape _ xs y zs) =
    a == y || List.member a xs || List.member a zs


{-| Determine if all elements satisfy some test.

    create ["a", "b"] "c" ["d", "e"]
        |> all (\x -> String.length x < 2)
        --> True

-}
all : (a -> Bool) -> Tape a -> Bool
all pred (Tape _ xs y zs) =
    pred y && List.all pred xs && List.all pred zs


{-| Determine if any element satisfy some test.

    create ["a", "b"] "c" ["d", "e"]
        |> any (\x -> x == "e")
        --> True

-}
any : (a -> Bool) -> Tape a -> Bool
any pred (Tape _ xs y zs) =
    pred y || List.any pred xs || List.any pred zs


{-| Find the maximum element in a non-empty list.
-}
maximum : Tape comparable -> comparable
maximum =
    reduce max


{-| Find the minimum element in a non-empty list.
-}
minimum : Tape comparable -> comparable
minimum =
    reduce min


{-| Get the sum of the list elements.
-}
sum : Tape number -> number
sum =
    reduce (+)


{-| Get the product of the list elements.
-}
product : Tape number -> number
product =
    reduce (*)


{-| Separate the list in the (left, head, tail) parts.

The parts can be assembled back together with `create left head tail`

-}
parts : Tape a -> ( List a, a, List a )
parts (Tape _ xs y zs) =
    ( List.reverse xs, y, zs )


{-| True if tape has no elements to the left o head
-}
isEmptyLeft : Tape a -> Bool
isEmptyLeft (Tape n _ _ _) =
    n == 0


{-| True if tape has no elements to the right o head
-}
isEmptyRight : Tape a -> Bool
isEmptyRight (Tape _ _ _ xs) =
    List.isEmpty xs


{-| Return index of element in tape

Elements left of head have negative indexes.

-}
elemIndex : a -> Tape a -> Maybe Int
elemIndex elem (Tape _ xs y zs) =
    case List.elemIndex elem xs of
        Just i ->
            Just (-1 - i)

        Nothing ->
            if elem == y then
                Just 0

            else
                List.elemIndex elem zs



--------------------------------------------------------------------------------
-- Auxiliary functions
--------------------------------------------------------------------------------


{-| Show tape as string, with emphasis on head element
-}
show : (a -> String) -> Tape a -> String
show f tape =
    case map f tape of
        Tape _ xs y zs ->
            let
                body =
                    List.reverse (("=> " ++ y) :: xs) ++ zs
            in
            "[ " ++ String.join ", " body ++ " ]"
