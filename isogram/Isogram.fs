module Isogram

let isIsogram (str:string) = 
    let cleanString = str.Replace(" ", "").Replace("-", "").ToUpper()
    let isogramSet = cleanString.ToCharArray() |> Set.ofArray
    isogramSet.Count = cleanString.Length