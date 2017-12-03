module Maxdeviant.AdventOfCode2017.Day3

open System

type Move =
  | None
  | Up
  | Down
  | Left
  | Right

let makeSpiral maxValue =
  let size = (float maxValue) |> Math.Sqrt |> Math.Ceiling |> int
  let center = (size / 2, size / 2)

  let getDistance ((x1, y1): int * int) ((x2, y2): int * int) =
    (float (x2 - x1) ** 2.0 + float (y2 - y1) ** 2.0)
    |> Math.Sqrt

  let isPositionInBounds (spiral: int[,]) (x, y) =
    x >= 0 && x < (Array2D.length2 spiral) && y >= 0 && y < (Array2D.length1 spiral)

  let isPositionEmpty (spiral: int[,]) (x, y) =
    spiral.[y, x] = 0

  let isValidPosition (spiral: int[,]) position =
    isPositionInBounds spiral position && isPositionEmpty spiral position

  let getPotentialMoves (spiral: int[,]) (x, y) =
    [(Left, (x + 1, y))
     (Up, (x, y - 1))
     (Down, (x - 1, y))
     (Right, (x, y + 1))]
     |> List.filter (fun (_, position) -> isValidPosition spiral position)

  let getBestMove (spiral: int[,]) (x, y) =
    getPotentialMoves spiral (x, y)
    |> List.sortBy (fun (_, position) -> getDistance center position)
    |> List.tryHead

  let getNextPosition (spiral: int[,]) position =
    match getBestMove spiral position with
    | Some (_, position) -> position
    | Option.None -> raise (Exception (sprintf "No valid moves from %A" position))

  let getNextMove = function
    | None -> Right
    | Up -> Left
    | Down -> Right
    | Left -> Down
    | Right -> Up

  let rec fillSpiral target (spiral: int[,]) n (x, y) currentMove =
    spiral.[y, x] <- n
    match n with
    | n when n = target -> spiral
    | n -> fillSpiral target spiral (n + 1) (getNextPosition spiral (x, y)) (getNextMove currentMove)

  fillSpiral maxValue (Array2D.init size size (fun _ _ -> 0)) 1 center None
