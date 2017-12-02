#load "CorruptionChecksum.fsx"

open Maxdeviant.AdventOfCode2017.Day2

open System.IO

let puzzleInput = File.ReadAllText("2017/src/02/input.txt")

let partOne = computeChecksum partOneChecksum

partOne puzzleInput

checksumTwo puzzleInput