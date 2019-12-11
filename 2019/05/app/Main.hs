module Main where

import Lib

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn "Part One"
  partOne input
  putStrLn "Part Two"
  partTwo input
