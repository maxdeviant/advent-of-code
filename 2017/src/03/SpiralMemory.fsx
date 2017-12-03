module Maxdeviant.AdventOfCode2017.Day3

open System

let isPositionInBounds (spiral: int[,]) (x, y) =
  x >= 0 && x < (Array2D.length2 spiral) && y >= 0 && y < (Array2D.length1 spiral)

let isPositionEmpty (spiral: int[,]) (x, y) =
  spiral.[y, x] = 0

let getPotentialMoves isValidMove (x, y) =
  [(x + 1, y)
   (x, y - 1)
   (x - 1, y)
   (x, y + 1)]
   |> List.filter isValidMove

let getBestMove isValidMove rankMove (x, y) =
  getPotentialMoves isValidMove (x, y)
  |> List.sortBy rankMove
  |> List.tryHead

let makeSpiral maxValue =
  let size = (float maxValue) |> Math.Sqrt |> Math.Ceiling |> int
  let center = (size / 2, size / 2)

  let getDistance ((x1, y1): int * int) ((x2, y2): int * int) =
    (float (x2 - x1) ** 2.0 + float (y2 - y1) ** 2.0)
    |> Math.Sqrt

  let isValidPosition (spiral: int[,]) position =
    isPositionInBounds spiral position && isPositionEmpty spiral position

  let getNextPosition (spiral: int[,]) position =
    match getBestMove (isValidPosition spiral) (getDistance center) position with
    | Some position -> position
    | None -> raise (Exception (sprintf "No valid moves from %A" position))

  let rec fillSpiral target (spiral: int[,]) n (x, y) =
    spiral.[y, x] <- n
    match n with
    | n when n = target -> spiral, center, (x, y)
    | n -> fillSpiral target spiral (n + 1) (getNextPosition spiral (x, y))

  fillSpiral maxValue (Array2D.init size size (fun _ _ -> 0)) 1 center

let countRequiredSteps spiral start goal =
  let getManhattanDistance (x1, y1) (x2, y2) =
    abs (x1 - x2) + abs (y1 - y2)

  let isValidPosition = isPositionInBounds spiral

  let getNextPosition position =
    match getBestMove isValidPosition (fun p -> getManhattanDistance goal p) position with
    | Some position -> position
    | None -> raise (Exception (sprintf "No valid moves from %A" position))

  let rec countSteps steps position =
    match position with
    | position when position = goal -> steps
    | position -> countSteps (steps + 1) (getNextPosition position)

  countSteps 0 start
