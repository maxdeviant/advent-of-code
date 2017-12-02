module Maxdeviant.AdventOfCode2017.Day1

open System

let charToInt = Char.GetNumericValue >> int

let getDigits (input: string) =
  input.ToCharArray()
  |> Array.map charToInt
  |> List.ofArray

let solveCaptcha solver (input: string) =
  getDigits input
  |> solver

let partOneSolver digits =
  let firstDigit = List.head digits

  let sumIf acc x y =
    if x = y then x + acc else acc

  let rec solve acc = function
  | [] -> acc
  | [x] -> sumIf acc x firstDigit
  | x :: y :: ys -> solve (sumIf acc x y) (y :: ys)

  solve 0 digits
