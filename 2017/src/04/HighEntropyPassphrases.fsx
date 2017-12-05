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

let isValidPassphrase validate =
  getWords
  >> validate

let countValidPassphrases validate =
  List.filter validate
  >> List.length
