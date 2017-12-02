#load "InverseCaptcha.fsx"

open Maxdeviant.AdventOfCode2017.Day1
open System.IO

let puzzleInput = (Path.Combine (__SOURCE_DIRECTORY__, "input.txt")) |> File.ReadAllText

let partOne = solveCaptcha partOneSolver

partOne "1122"
partOne "1111"
partOne "1234"
partOne "91212129"

partOne puzzleInput

let partTwo = solveCaptcha partTwoSolver

partTwo "1212"
partTwo "1221"
partTwo "123425"
partTwo "123123"
partTwo "12131415"

partTwo puzzleInput
