module PhoneNumber

let stringToInt str = str |> uint64

let letters = ['a'..'z'] @ ['A'..'Z']
let notAllowedCharacters = ['@';':';'!']

let checkIfStringContainAnyLetterOf (str: string) (lst: List<char>): bool =
    (Set.intersect (str.ToCharArray() |> Set.ofArray) (Set.ofList lst)).Count <> 0

let removeFirstCharacterIfLengthEquals11 (str: string) =
    if str.Length = 11 then str.[1..] else str

let clean (input: string): Result<uint64, string> = 
    let clearInput = input.Replace("+", "").Replace(".", "").Replace("(", "").Replace(")", "").Replace("-", "").Replace(" ", "")
    match String.length clearInput with
    | x when x > 11 -> Error "more than 11 digits"
    | x when x < 10 -> Error "incorrect number of digits"
    | 11 when not (clearInput.StartsWith("1")) -> Error "11 digits must start with 1"
    | _ -> 
        match clearInput with 
        | x when checkIfStringContainAnyLetterOf x letters -> Error "letters not permitted"
        | x when checkIfStringContainAnyLetterOf x notAllowedCharacters -> Error "punctuations not permitted"
        | x when (x.ToCharArray() |> Array.rev).[9] = '0' -> Error "area code cannot start with zero"
        | x when (x.ToCharArray() |> Array.rev).[9] = '1' -> Error "area code cannot start with one"
        | x when (x.ToCharArray() |> Array.rev).[6] = '0' -> Error "exchange code cannot start with zero"
        | x when (x.ToCharArray() |> Array.rev).[6] = '1' -> Error "exchange code cannot start with one"
        | _ -> clearInput |> removeFirstCharacterIfLengthEquals11 |> stringToInt |> Ok