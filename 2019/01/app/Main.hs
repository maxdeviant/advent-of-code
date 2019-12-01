module Main where

import Control.Arrow ((>>>))
import Lib

partOne =
  lines >>> map read >>> map requiredFuel >>> foldr (+) 0

main :: IO ()
main = do
  input <- readFile "input.txt"
  print $ partOne input
