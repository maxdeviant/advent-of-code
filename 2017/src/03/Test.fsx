#load "SpiralMemory.fsx"

open Maxdeviant.AdventOfCode2017.Day3
open System.IO

let puzzleInput = (Path.Combine (__SOURCE_DIRECTORY__, "input.txt")) |> File.ReadAllText

let partOne (input: string) =
  let n = int input
  let spiral, start, goal = makeSpiral n
  countRequiredSteps spiral start goal

partOne "1"
partOne "9"
partOne "23"
partOne "25"
partOne "1024"

partOne puzzleInput
