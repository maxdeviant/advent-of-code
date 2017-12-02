module Maxdeviant.AdventOfCode2017.Day2

let parseInput (input: string) =
  input.Split('\n')
  |> Seq.map (fun row -> row.Split('\t') |> Seq.toList)
  |> Seq.toList

let computeChecksum algorithm =
  parseInput >> algorithm

let partOneChecksum (rows: string list list) =
  let checksum =
    List.map int
    >> List.sortDescending
    >> (fun xs ->
      let max = List.head xs
      let min = List.head (List.rev xs)
      max - min)

  rows |> List.sumBy checksum

let partTwoChecksum (rows: string list list) =
  let parseColumns = List.map float

  let isWholeNumber n = (n % 1.0) = 0.0

  let checksum columns =
    let columns' = columns |> List.map float
    columns'
    |> List.collect (fun x -> columns' |> List.map (fun y -> (x, y)))
    |> List.map (fun (x, y) -> x / y)
    |> List.filter isWholeNumber
    |> List.sortDescending
    |> List.head

  rows
  |> List.sumBy checksum
  |> int
