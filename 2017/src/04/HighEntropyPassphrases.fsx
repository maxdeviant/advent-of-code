module Maxdeviant.AdventOfCode2017.Day4

open System

let isValidPassphrase passphrase =
  let getWords (passphrase: string) =
    passphrase.Split(' ')
    |> List.ofArray

  let rec isUnique unique = function
    | [] -> true
    | x :: _ when Set.contains x unique -> false
    | x :: xs -> isUnique (Set.add x unique) xs

  passphrase
  |> getWords
  |> isUnique Set.empty

let countValidPassphrases =
  List.filter isValidPassphrase
  >> List.length
