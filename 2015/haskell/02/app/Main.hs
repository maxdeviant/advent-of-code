module Main where

import qualified Day2 (partOne, partTwo)

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn "Part One"
  case Day2.partOne input of
    Right answer -> print answer
    Left error -> print $ "Failed with: " ++ error
  putStrLn "Part Two"
  case Day2.partTwo input of
    Right answer -> print answer
    Left error -> print $ "Failed with: " ++ error
