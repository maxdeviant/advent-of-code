#load "SpiralMemory.fsx"

open Maxdeviant.AdventOfCode2017.Day3
open System.IO

let puzzleInput = (Path.Combine (__SOURCE_DIRECTORY__, "input.txt")) |> File.ReadAllText
