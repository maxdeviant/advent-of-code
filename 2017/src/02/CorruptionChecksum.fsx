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
    |> List.sort
    |> (fun x ->
      let head = List.head x
      let tail = List.head (List.rev x)
      tail - head)

  input
  |> parseInput
  |> List.map checksum'
  |> List.sum