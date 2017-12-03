module Maxdeviant.AdventOfCode2017.Day3

open System

type Move =
  | None
  | Up
  | Down
  | Left
  | Right

// R U L L D D R R R U U U

let makeSpiral maxValue =
  let size = (float maxValue) |> Math.Sqrt |> Math.Ceiling |> int

  let getNextMove = function
    | None -> Right
    | Up -> Left
    | Down -> Right
    | Left -> Down
    | Right -> Up

  let isSquareEmpty (spiral: int[,]) (x, y) =
    spiral.[y, x] = 0

  let rec move (spiral: int[,]) currentMove (x, y) =
    let nextPosition =
      match currentMove with
      | None -> (x, y)
      | Up -> (x, y - 1)
      | Down -> (x, y + 1)
      | Left -> (x - 1, y)
      | Right -> (x + 1, y)
    if isSquareEmpty spiral nextPosition
      then nextPosition
      else move spiral (getNextMove currentMove) nextPosition

  let rec fillSpiral target (spiral: int[,]) n (x, y) currentMove =
    spiral.[y, x] <- n
    printfn "%A %A %A %A" (x, y) currentMove (getNextMove currentMove) spiral
    match n with
    | n when n = target -> spiral
    | n -> fillSpiral target spiral (n + 1) (move spiral currentMove (x, y)) (getNextMove currentMove)

  let start = (size / 2, size / 2)

  fillSpiral maxValue (Array2D.init size size (fun _ _ -> 0)) 1 start None
