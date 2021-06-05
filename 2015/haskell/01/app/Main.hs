module Main where

import qualified Day1 (partOne, partTwo)

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn "Part One"
  print $ Day1.partOne input
  putStrLn "Part Two"
  print $ Day1.partTwo input
