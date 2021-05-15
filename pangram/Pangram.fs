module Pangram

let alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ".ToCharArray();

let isPangram (input: string): bool = 
    let inputAsSet = input.ToUpper() |> Set.ofSeq
    Set.intersect inputAsSet (alpha |> Set.ofArray) = (alpha |> Set.ofArray)