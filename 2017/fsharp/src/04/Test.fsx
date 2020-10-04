#load "HighEntropyPassphrases.fsx"

open Maxdeviant.AdventOfCode2017.Day4
open System.IO

let puzzleInput = (Path.Combine (__SOURCE_DIRECTORY__, "input.txt")) |> File.ReadAllText

let partOne (input: string) =
  input.Trim().Split('\n')
  |> List.ofArray
  |> countValidPassphrases (isValidPassphrase isUnique)

partOne "aa bb cc dd ee"
partOne "aa bb cc dd aa"
partOne "aa bb cc dd aaa"

partOne puzzleInput

let partTwo (input: string) =
  input.Trim().Split('\n')
  |> List.ofArray
  |> countValidPassphrases (isValidPassphrase (fun p -> isUnique p && containsNoAnagrams p))

partTwo "abcde fghij"
partTwo "abcde xyz ecdab"
partTwo "a ab abc abd abf abj"
partTwo "iiii oiii ooii oooi oooo"
partTwo "oiii ioii iioi iii"

partTwo puzzleInput
