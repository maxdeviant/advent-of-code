module Maxdeviant.AdventOfCode2017.Day1

open System

let charToInt = Char.GetNumericValue >> int

let getDigits (input: string) =
  input.ToCharArray()
  |> Array.map charToInt
  |> List.ofArray

let solveCaptcha solver input =
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

let partTwoSolver (digits: int list) =
  assert ((List.length digits) % 2 = 0)

  let halfwayDistance = (List.length digits) / 2

  let rec halfway position ahead =
    match ahead with
    | [] -> halfway position digits
    | x :: xs ->
      if position = 1
      then x
      else halfway (position - 1) xs

  digits
  |> List.indexed
  |> List.map (fun (i, x) ->
    (x, halfway halfwayDistance (digits |> Seq.skip (i + 1) |> Seq.toList)))
  |> List.filter (fun (x, y) -> x = y)
  |> List.sumBy (fun (x, _) -> x)
