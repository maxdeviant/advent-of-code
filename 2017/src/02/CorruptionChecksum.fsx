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

let checksumTwo (input: string) =
  let checksum' (columns: string list) =
    columns
    |> List.map float
    |> List.collect (fun x -> columns |> List.map float |> List.map (fun y -> (x, y)))
    |> List.map (fun (x, y) -> x / y)
    |> List.filter (fun x -> (x % 1.0) = 0.0)
    |> List.sortDescending
    |> List.head

  input
  |> parseInput
  |> List.map checksum'
  |> List.sum