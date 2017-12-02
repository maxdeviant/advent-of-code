module Maxdeviant.AdventOfCode2017.Day2

open System

let parseInput (input: string) =
  input.Split('\n')
  |> Seq.map (fun row -> row.Split('\t') |> Seq.toList)
  |> Seq.toList

let checksum (input: string) =
  let checksum' (columns: string list) =
    columns
    |> List.map int
    |> List.sortDescending
    |> (fun x ->
      let max = List.head x
      let min = List.head (List.rev x)
      max - min)

  input
  |> parseInput
  |> List.map checksum'
  |> List.sum

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