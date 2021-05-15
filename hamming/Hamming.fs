module Hamming

let rec distanceAux (str1: string) (str2: string) (idx:int)  (acc:int): int = 
    match idx = str1.Length with
    | true -> acc 
    | false -> 
        match str1.[idx] = str2.[idx] with
        | true -> distanceAux str1 str2 (idx + 1) acc
        | false -> distanceAux str1 str2 (idx + 1) (acc + 1)

let findDifferenceBetweenEqualsLengthStrings (str1: string) (str2: string): int = 
    distanceAux str1 str2 0 0
    
let distance (strand1: string) (strand2: string): int option = 
    match strand1.Length = strand2.Length with
    | false -> None
    | true -> Some(findDifferenceBetweenEqualsLengthStrings strand1 strand2)

  