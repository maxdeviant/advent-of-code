module Lib
  ( partOne
  , digits
  ) where

import Data.List.Split (splitOn)

digits :: Integral a => a -> [a]
digits 0 = []
digits n = digits (n `div` 10) ++ [n `mod` 10]

containsDoubles :: Int -> Bool
containsDoubles n = containsDoubles' $ digits n
  where
    containsDoubles' [] = False
    containsDoubles' (_:[]) = False
    containsDoubles' (x:y:rest) =
      if x == y
        then True
        else containsDoubles' (y : rest)

neverDecreases :: Int -> Bool
neverDecreases n = neverDecreases' $ digits n
  where
    neverDecreases' [] = True
    neverDecreases' (_:[]) = True
    neverDecreases' (x:y:rest) =
      if y < x
        then False
        else neverDecreases' (y : rest)

possiblePasswords :: Int -> Int -> [Int]
possiblePasswords lower upper =
  filter containsDoubles . filter neverDecreases $ allPasswords
  where
    allPasswords = [lower .. upper]

partOne :: String -> Int
partOne input = length $ possiblePasswords (read lower) (read upper)
  where
    (lower:upper:[]) = splitOn "-" input
