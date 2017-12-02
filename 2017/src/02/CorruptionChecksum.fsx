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