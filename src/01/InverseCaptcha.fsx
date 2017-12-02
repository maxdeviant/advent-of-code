module Maxdeviant.AdventOfCode2017.Day1

open System

let charToInt = Char.GetNumericValue >> int

let getDigits (input: string) =
  input.ToCharArray()
  |> Array.map charToInt
  |> List.ofArray

let sumIfMatch digits =
  let rec sum acc = function
  | [_] | [] -> acc
  | x :: y :: ys ->
    sum (acc + (if x = y then x else 0)) (y :: ys)

  sum 0 digits

let solveCaptcha (input: string) =
  let firstDigit = string(input.Chars 0)
  getDigits (input + firstDigit)
  |> sumIfMatch
