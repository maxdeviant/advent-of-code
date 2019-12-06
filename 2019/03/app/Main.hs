module Main where

import Lib

main :: IO ()
main = do
  input <- readFile "input.txt"
  print "Part One"
  print $ partOne input
  print "Part Two"
  print $ partTwo input
