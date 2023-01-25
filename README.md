# Tape for ELM 

`elm-tape` implements a tape data structure in ELM. Tapes are linear data structures with a movable cursor.


# Examples

We can convert tapes from/to lists:

    tape = Tape.create [] 0 [1, 2, 3]
    lst = Tape.toList tape

We can move the cursor on the tape