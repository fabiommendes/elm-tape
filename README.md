# Tape for ELM

`elm-tape` implements a tape data structure in ELM. Tapes are linear data structures with a movable cursor.



## Examples

Tapes appear naturally in models that show a collection of objects, but focus into a single at each time, such as a carousel,

```elm
type alias Model =
    { tape : Tape Item
      -- other fields...
    }

update msg m =
    GoNext ->
        { m | tape = Tape.rightCycle m.tape }

    GoPrevious ->
        { m | tape = Tape.leftCycle m.tape }

    GoTo n ->
        { m | tape = Tape.rightN (n - position m.tape) m.tape }

    Update data ->
        { m | tape = Tape.write data tape }

    -- other messages ...
```

A similar pattern is found when we want to implement Undo/Redo for a model or
parts of the model

```elm
type alias Model = Tape State

update msg history =
    case msg of
        Undo ->
            history
                |> Tape.undo

        Redo ->
            history
                |> Tape.redo

        Save state ->
            Tape.pushRight state history

        --- other messages ...
```

And, of course, it also has som more exotic applications, such as this partial [Brainfuck](https://en.wikipedia.org/wiki/Brainfuck) interpreter ;-)

```elm
process cmd tape =
    case cmd of
        '+' ->
            Tape.mapHead (\n -> modBy 256 (n + 1))

        '-' ->
            Tape.mapHead (\n -> modBy 256 (n - 1))

        '>' ->
            Tape.rightWithDefault 0 tape

        '<' ->
            Tape.moveLeft tape

        -- '[' and ']' left as an exercise to the reader...
```
