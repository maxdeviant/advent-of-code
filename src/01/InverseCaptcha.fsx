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
  let sumIf acc x y =
    if x = y then x + acc else acc

  let rec solve firstDigit acc = function
  | [] -> acc
  | [x] -> sumIf acc x firstDigit
  | x :: y :: ys -> solve firstDigit (sumIf acc x y) (y :: ys)

  solve (List.head digits) 0 digits
