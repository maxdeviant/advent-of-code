module Maxdeviant.AdventOfCode2017.Day4

let getWords (passphrase: string) =
  passphrase.Split(' ')
  |> List.ofArray

let isUnique (values: string list) =
  let rec isUnique' unique = function
    | [] -> true
    | x :: _ when Set.contains x unique -> false
    | x :: xs -> isUnique' (Set.add x unique) xs

  isUnique' Set.empty values

let countCharacters (word: string) =
  let rec countCharacters' counts characters =
    match characters with
    | [] -> counts
    | x :: xs ->
      match Map.tryFind x counts with
      | Some count -> countCharacters' (Map.add x (count + 1) counts) xs
      | None -> countCharacters' (Map.add x 1 counts) xs


  word.Trim().ToCharArray()
  |> List.ofArray
  |> countCharacters' Map.empty

let countAnagrams values =
  values
  |> List.filter (fun x -> List.exists (fun y -> x <> y && (countCharacters x) = (countCharacters y)) values)
  |> List.length

let containsNoAnagrams values =
  (countAnagrams values) = 0

let isValidPassphrase validate =
  getWords
  >> validate

let countValidPassphrases validate =
  List.filter validate
  >> List.length
