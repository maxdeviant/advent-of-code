#load "SpiralMemory.fsx"

open Maxdeviant.AdventOfCode2017.Day3
open System.IO

let puzzleInput = (Path.Combine (__SOURCE_DIRECTORY__, "input.txt")) |> File.ReadAllText

let partOne (input: string) =
  let n = int input
  let spiral = makeSpiral n
  printfn "%A" spiral
  countRequiredSteps spiral n

partOne "1"
partOne "12"
partOne "25"
partOne "1024"

partOne puzzleInput

