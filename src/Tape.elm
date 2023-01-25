module Tape exposing
    ( Tape
    , create, fromList, fromListOrDefault, fromTails, singleton, repeat, range
    , read, write, advance, rewind, left, right, tryLeft, tryRight
    , rightN, leftWhile, rightWhile, leftWithDefault, rightWithDefault, moveLeft, moveRight, leftCycle, rightCycle, leftCycleN, rightCycleN, pushLeft, pushRight
    , map, map2, map3, mapHead, mapLeft, mapRight, mapSelect, indexedMap, reverse, sort, sortBy, sortWith, intersperse, toList, unzip
    , foldr, foldl, reduce, maximum, minimum, sum, product, all, any
    , filterLeft, filterRight
    , length, position, member, isEmptyLeft, isEmptyRight, parts, leftList, leftTail, rightList, rightTail, elemIndex
    )

{-| A linear structure with a cursor pointing to some specific element. Tapes cannot be empty.

@docs Tape


## Create

@docs create, fromList, fromListOrDefault, fromTails, singleton, repeat, range


## Simple tape operators

@docs read, write, advance, rewind, left, right, tryLeft, tryRight


## Advanced tape operators

@docs leftN, rightN, leftWhile, rightWhile, leftWithDefault, rightWithDefault, moveLeft, moveRight, leftCycle, rightCycle, leftCycleN, rightCycleN, pushLeft, pushRight


## Transforms

@docs map, map2, map3, mapHead, mapLeft, mapRight, mapSelect, indexedMap, reverse, sort, sortBy, sortWith, intersperse, toList, unzip


## Summarizing and folding

@docs foldr, foldl, reduce, maximum, minimum, sum, product, all, any


## Filtering

@docs filterLeft, filterRight


## Properties

@docs length, position, member, isEmptyLeft, isEmptyRight, parts, leftList, leftTail, rightList, rightTail, elemIndex


## Examples

A partial [Brainfuck](https://en.wikipedia.org/wiki/Brainfuck) interpreter

    process cmd tape =
        case cmd of
            '+' ->
                mapHead (\n -> modBy 256 (n + 1))

            '-' ->
                mapHead (\n -> modBy 256 (n - 1))

            '>' ->
                rightWithDefault 0 tape

            '<' ->
                left tape

            _ ->
                -- '[' and ']' left as exercise to the reader ;-)
                tape

A carousel model

    type alias Model =
        { tape : Tape Item
          -- Other fields...
        }

    update msg m =
        OnMoveRight ->
            { m | tape = rightCycle m.tape }

        OnMoveLeft ->
            { m | tape = leftCycle m.tape }

        OnMoveTo n ->
            { m | tape = rightN (n - position m.tape) m.tape }

        OnUpdateCurrent data ->
            { m | tape = mapHead (updateWith data) tape }

        _ ->
            -- Handle other messages ...
            m

-}

import List.Extra as List
import Maybe.Extra as Maybe


{-| Base iterator type
-}
type Tape a
    = Tape (List a) a (List a)



--------------------------------------------------------------------------------
-- CREATE
--------------------------------------------------------------------------------


{-| Create a tape from the left, head, and right parts.

The tape is conceptually equivalent to the list `left ++ [head] ++ right`
and has a cursor that points to the tape head.

-}
create : List a -> a -> List a -> Tape a
create xs y zs =
    Tape (List.reverse xs) y zs


{-| Try to create tape from List. Empty lists return Nothing.
-}
fromList : List a -> Maybe (Tape a)
fromList xs =
    case xs of
        x :: ys ->
            Just (Tape [] x ys)

        _ ->
            Nothing


{-| Create a tape from List or a singleton tape with the given default value.
-}
fromListOrDefault : a -> List a -> Tape a
fromListOrDefault x xs =
    case xs of
        y :: ys ->
            Tape [] y ys

        _ ->
            singleton x


{-| Create a tape with only one element
-}
singleton : a -> Tape a
singleton a =
    Tape [] a []


{-| Create a tape with n copies of a value.

Empty tapes or negative n are ignored.

-}
repeat : Int -> a -> Tape a
repeat n x =
    Tape [] x (List.repeat (n - 1) x)


{-| Create a tape of numbers, every element increasing by one. You give the lowest and highest number that should be in the tape.
-}
range : Int -> Int -> Tape Int
range a b =
    if b > a then
        Tape [] a (List.range (a + 1) b)

    else
        singleton a



--------------------------------------------------------------------------------
-- TAPE OPERATIONS
--------------------------------------------------------------------------------


{-| Read element on the current position.
-}
read : Tape a -> a
read (Tape _ x _) =
    x


{-| Write element to the tape's current position.
-}
write : a -> Tape a -> Tape a
write y (Tape xs _ zs) =
    Tape xs y zs


{-| Write element to the tape's current position and push head to the left.
-}
pushLeft : a -> Tape a -> Tape a
pushLeft new (Tape xs y zs) =
    Tape (y :: xs) new zs


{-| Write element to the tape's current position and push head to the right.
-}
pushRight : a -> Tape a -> Tape a
pushRight new (Tape xs y zs) =
    Tape xs new (y :: zs)


{-| Move head a single position to the left, if possibList.
-}
left : Tape a -> Tape a
left tape =
    tryLeft tape |> Maybe.withDefault tape


{-| Move left while predicate holds true.
-}
leftWhile : (a -> Bool) -> Tape a -> Tape a
leftWhile pred ((Tape _ y _) as tape) =
    if pred y then
        rightWhile pred (left tape)

    else
        tape


{-| Move head a single position to the left, adding default if an empty space is found.
-}
leftWithDefault : a -> Tape a -> Tape a
leftWithDefault default ((Tape _ y zs) as tape) =
    tryLeft tape |> Maybe.withDefaultLazy (\_ -> Tape [] default (y :: zs))


{-| Try to move a single position to the left.
-}
tryLeft : Tape a -> Maybe (Tape a)
tryLeft (Tape xs y zs) =
    case xs of
        [] ->
            Nothing

        w :: ws ->
            Just (Tape ws w (y :: zs))


{-| Move head a single position to the left, and cycles to the back if head is in the starting position.
-}
leftCycle : Tape a -> Tape a
leftCycle tape =
    tryLeft tape |> Maybe.withDefaultLazy (\_ -> advance tape)


{-| Move at most n positions to the left.

Negative steps move to the right.

-}
leftN : Int -> Tape a -> Tape a
leftN n ((Tape xs _ zs) as tape) =
    if n > 0 then
        case List.splitAt n xs of
            ( [], _ ) ->
                tape

            ( w :: ws, toLeft ) ->
                Tape toLeft w (List.reverse ws ++ zs)

    else if n < 0 then
        case List.splitAt -n zs of
            ( [], _ ) ->
                tape

            ( w :: ws, toRight ) ->
                Tape (List.reverse ws ++ xs) w toRight

    else
        tape


{-| Move head n positions to the left, cycling if necessary.
-}
leftCycleN : Int -> Tape a -> Tape a
leftCycleN n tape =
    if n > 0 then
        callN n leftCycle tape

    else if n < 0 then
        callN n rightCycle tape

    else
        tape


{-| Move head a single position to the right, if possible
-}
right : Tape a -> Tape a
right tape =
    tryRight tape |> Maybe.withDefault tape


{-| Move right while predicate holds true.
-}
rightWhile : (a -> Bool) -> Tape a -> Tape a
rightWhile pred ((Tape _ y _) as tape) =
    if pred y then
        rightWhile pred (right tape)

    else
        tape


{-| Move head a single position to the right, adding default if an empty space is found.
-}
rightWithDefault : a -> Tape a -> Tape a
rightWithDefault default ((Tape xs y _) as tape) =
    tryRight tape |> Maybe.withDefaultLazy (\_ -> Tape (y :: xs) default [])


{-| Try to move a single position to the right.
-}
tryRight : Tape a -> Maybe (Tape a)
tryRight (Tape xs y zs) =
    case zs of
        [] ->
            Nothing

        w :: ws ->
            Just (Tape (y :: xs) w ws)


{-| Move head a single position to the right, and cycles to the front if head is in the last position.
-}
rightCycle : Tape a -> Tape a
rightCycle tape =
    tryRight tape |> Maybe.withDefaultLazy (\_ -> rewind tape)


{-| Move at most n positions to the right.

Negative values move to the left.

-}
rightN : Int -> Tape a -> Tape a
rightN n =
    leftN -n


{-| Move head n positions to the right, cycling if necessary.
-}
rightCycleN : Int -> Tape a -> Tape a
rightCycleN n =
    leftCycleN -n


{-| Rewind tape so the head points to the first element
-}
rewind : Tape a -> Tape a
rewind ((Tape xs y zs) as tape) =
    case xs of
        [] ->
            tape

        w :: ws ->
            rewind (Tape ws w (y :: zs))


{-| Advance tape so the head points to the last element
-}
advance : Tape a -> Tape a
advance ((Tape xs y zs) as tape) =
    case zs of
        [] ->
            tape

        w :: ws ->
            advance (Tape (y :: xs) w ws)


{-| Move a single position to the left carrying the head value with the cursor
-}
moveLeft : Tape a -> Tape a
moveLeft ((Tape xs y zs) as tape) =
    case xs of
        [] ->
            tape

        w :: ws ->
            Tape ws y (w :: zs)


{-| Move a single position to the right carrying the head value with the cursor
-}
moveRight : Tape a -> Tape a
moveRight ((Tape xs y zs) as tape) =
    case zs of
        [] ->
            tape

        w :: ws ->
            Tape (w :: xs) y ws



--------------------------------------------------------------------------------
-- TRANSFORM
--------------------------------------------------------------------------------


{-| Apply a function to every element of a tape.

It is possible to map specific parts of the tape.

    map f tape
        -- order of application does not matter here
        ==> (tape
                |> mapLeft f
                |> mapHead f
                |> mapRight f
            )

-}
map : (a -> b) -> Tape a -> Tape b
map f (Tape xs y zs) =
    Tape (List.map f xs) (f y) (List.map f zs)


{-| Combine two tapes with function. It keeps the smallest left and right part of each tape.
-}
map2 : (a -> b -> result) -> Tape a -> Tape b -> Tape result
map2 f (Tape xs y zs) (Tape xs_ y_ zs_) =
    Tape (List.map2 f xs xs_) (f y y_) (List.map2 f zs zs_)


{-| Combine three tapes with function. It keeps the smallest left and right part of each tape.
-}
map3 : (a -> b -> c -> result) -> Tape a -> Tape b -> Tape c -> Tape result
map3 f (Tape xs y zs) (Tape xs_ y_ zs_) (Tape xs__ y__ zs__) =
    Tape (List.map3 f xs xs_ xs__) (f y y_ y__) (List.map3 f zs zs_ zs__)


{-| Modify head by given function.
-}
mapHead : (a -> a) -> Tape a -> Tape a
mapHead f (Tape xs y zs) =
    Tape xs (f y) zs


{-| Modify everything right of the head (head not included).
-}
mapRight : (a -> a) -> Tape a -> Tape a
mapRight f (Tape xs y zs) =
    Tape xs y (List.map f zs)


{-| Modify everything left of the head (head not included).
-}
mapLeft : (a -> a) -> Tape a -> Tape a
mapLeft f (Tape xs y zs) =
    Tape (List.map f xs) y zs


{-| Map function passing a boolean that tells if each element is the head or not.
-}
mapSelect : (Bool -> a -> b) -> Tape a -> Tape b
mapSelect f (Tape xs y zs) =
    Tape (List.map (f False) xs) (f True y) (List.map (f False) zs)


{-| Places the given value between all members of the given tape.
-}
intersperse : a -> Tape a -> Tape a
intersperse x (Tape xs y zs) =
    Tape (x :: List.intersperse x xs) y (x :: List.intersperse x zs)


{-| Sort values from lowest to highest

Head moves to keep track of current element.

-}
sort : Tape comparable -> Tape comparable
sort =
    sortBy identity


{-| Sort values by a derived property.

Head moves to keep track of current element.

-}
sortBy : (a -> comparable) -> Tape a -> Tape a
sortBy f (Tape xs y zs) =
    let
        keyed =
            ( True, y ) :: (List.map (Tuple.pair False) xs ++ List.map (Tuple.pair False) zs)
    in
    keyed
        |> List.sortBy (\( _, x ) -> f x)
        |> fromListOrDefault ( False, y )
        -- (... but list will never be empty)
        |> rightWhile (\( key, _ ) -> not key)
        |> map Tuple.second


{-| Sort values with a custoem comparison function.

Head moves to keep track of current element.

-}
sortWith : (a -> a -> Order) -> Tape a -> Tape a
sortWith f (Tape xs h zs) =
    let
        keyed =
            ( True, h ) :: (List.map (Tuple.pair False) xs ++ List.map (Tuple.pair False) zs)
    in
    keyed
        |> List.sortWith (\( _, x ) ( _, y ) -> f x y)
        |> fromListOrDefault ( False, h )
        -- (... but list will never be empty)
        |> rightWhile (\( key, _ ) -> not key)
        |> map Tuple.second


{-| Decompose a list of tuples into a tuple of lists.
-}
unzip : Tape ( a, b ) -> ( Tape a, Tape b )
unzip (Tape xs ( a, b ) zs) =
    let
        ( xas, xbs ) =
            List.unzip xs

        ( zas, zbs ) =
            List.unzip zs
    in
    ( Tape xas a zas, Tape xbs b zbs )


{-| Same as map but the function is also applied to the index of each element.

The head has an index of zero and the left hand side of the tape is indexed with
negative numbers.

-}
indexedMap : (Int -> a -> b) -> Tape a -> Tape b
indexedMap f (Tape xs y zs) =
    let
        f1 n x =
            f -(n + 1) x

        f2 n x =
            f (n + 1) x
    in
    Tape (List.indexedMap f1 xs) (f 0 y) (List.indexedMap f2 zs)


{-| Reduce a tape from the left.

It has the same effect as reducing the equivalent list.

-}
foldl : (a -> b -> b) -> b -> Tape a -> b
foldl f acc (Tape xs y zs) =
    List.foldl f (List.foldr f acc xs) (y :: zs)


{-| Reduce a tape from the right.

It has the same effect as reducing the equivalent list.

-}
foldr : (a -> b -> b) -> b -> Tape a -> b
foldr f acc (Tape xs y zs) =
    List.foldl f (List.foldr f acc zs) (y :: xs)


{-| Reduce tape applying binary operator.

Similar to a fold, but it does not require an initial accumulator.

-}
reduce : (a -> a -> a) -> Tape a -> a
reduce op (Tape xs y zs) =
    List.foldl op (List.foldr op y zs) xs


{-| Converts tape to list
-}
toList : Tape a -> List a
toList (Tape xs y zs) =
    List.reverse xs ++ (y :: zs)


{-| Keep only elements to the left that pass predicate.
Right part is not touched.

This function does not affect the head.

-}
filterRight : (a -> Bool) -> Tape a -> Tape a
filterRight pred (Tape xs y zs) =
    Tape xs y (List.filter pred zs)


{-| Keep only elements to the right that pass predicate.
Right part is not touched.

This function does not affect the head.

-}
filterLeft : (a -> Bool) -> Tape a -> Tape a
filterLeft pred (Tape xs y zs) =
    Tape (List.filter pred xs) y zs



--------------------------------------------------------------------------------
-- UTILITIES
--------------------------------------------------------------------------------


{-| Determine the length of a tape.
-}
length : Tape a -> Int
length (Tape xs _ ys) =
    List.length xs + List.length ys + 1


{-| Determine the position of head.

Position corresponds to the number of steps we must move
right from a rewinded tape.

    n = (position tape)
    right n (rewind tape) ==> tape

-}
position : Tape a -> Int
position (Tape xs _ _) =
    List.length xs


{-| Reverse a list.
-}
reverse : Tape a -> Tape a
reverse (Tape xs y zs) =
    Tape zs y xs


{-| Figure out whether a tape contains a value.
-}
member : a -> Tape a -> Bool
member a (Tape xs y zs) =
    a == y || List.member a xs || List.member a zs


{-| Determine if all elements satisfy some test.
-}
all : (a -> Bool) -> Tape a -> Bool
all pred (Tape xs y zs) =
    pred y && List.all pred xs && List.all pred zs


{-| Determine if any elements satisfy some test.
-}
any : (a -> Bool) -> Tape a -> Bool
any pred (Tape xs y zs) =
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
parts (Tape xs y zs) =
    ( List.reverse xs, y, zs )


{-| Includes the head and everything to its left

The order of elements is reversed, so the head corresponds to the
first element.

    tape = create [1, 2] 3 [4, 5]
    leftList tape ==> [3, 2, 1]

-}
leftList : Tape a -> List a
leftList (Tape xs y _) =
    y :: xs


{-| Similar to leftList, but does not include the head.

    tape = create [1, 2] 3 [4, 5]
    leftTail tape ==> [2, 1]  -- Head is absent!

-}
leftTail : Tape a -> List a
leftTail (Tape xs y _) =
    y :: xs


{-| Includes the head and everything to its right
-}
rightList : Tape a -> List a
rightList (Tape _ y zs) =
    y :: zs


{-| Similar to rightList, but does not include the head.
-}
rightTail : Tape a -> List a
rightTail (Tape _ _ zs) =
    zs


{-| Construct tape from left and right tails.

    fromTails (leftTail tape) (head tape) (rightTail tape) ==> tape

-}
fromTails : List a -> a -> List a -> Tape a
fromTails =
    Tape


{-| True if tape has no elements to the left o head
-}
isEmptyLeft : Tape a -> Bool
isEmptyLeft (Tape xs _ _) =
    List.isEmpty xs


{-| True if tape has no elements to the right o head
-}
isEmptyRight : Tape a -> Bool
isEmptyRight (Tape _ _ xs) =
    List.isEmpty xs


{-| Return index of element in tape

Elements left of head have negative indexes.

-}
elemIndex : a -> Tape a -> Maybe Int
elemIndex elem (Tape xs y zs) =
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


callN : Int -> (a -> a) -> a -> a
callN n f x =
    if n <= 0 then
        x

    else if n == 1 then
        f x

    else
        callN (n - 1) f (f x)
