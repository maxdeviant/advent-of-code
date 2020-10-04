#load "CorruptionChecksum.fsx"

open Maxdeviant.AdventOfCode2017.Day2
open System.IO

let puzzleInput = (Path.Combine (__SOURCE_DIRECTORY__, "input.txt")) |> File.ReadAllText

let partOne = computeChecksum partOneChecksum

partOne puzzleInput

let partTwo = computeChecksum partTwoChecksum

partTwo puzzleInput