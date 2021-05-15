module QueenAttack

let validPositionIndexes = [|0..7|]

let create (position: int * int) = 
    if Array.contains (fst position) validPositionIndexes then Array.contains (snd position) validPositionIndexes else false

let canAttack (queen1: int * int) (queen2: int * int) = 
    if fst queen1 = fst queen2 || snd queen1 = snd queen2 then true 
    else abs (fst queen1 - fst queen2) = abs (snd queen1 - snd queen2)
