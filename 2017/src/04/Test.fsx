#load "HighEntropyPassphrases.fsx"

open Maxdeviant.AdventOfCode2017.Day4
open System.IO

let puzzleInput = (Path.Combine (__SOURCE_DIRECTORY__, "input.txt")) |> File.ReadAllText

let partOne (input: string) =
  input.Trim().Split('\n')
  |> List.ofArray
  |> countValidPassphrases

partOne "aa bb cc dd ee"
partOne "aa bb cc dd aa"
partOne "aa bb cc dd aaa"

partOne puzzleInput
