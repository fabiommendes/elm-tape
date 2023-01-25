# Tape for ELM 

`elm-tape` implements a tape data structure in ELM. Tapes are linear data structures with a movable cursor.


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
