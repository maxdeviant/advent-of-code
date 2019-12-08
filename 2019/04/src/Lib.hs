module Lib
  ( partOne
  , partTwo
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

containsIsolatedDoubles :: Int -> Bool
containsIsolatedDoubles n = containsIsolatedDoubles' (-1) $ digits n
  where
    containsIsolatedDoubles' _ [] = False
    containsIsolatedDoubles' _ (_:[]) = False
    containsIsolatedDoubles' lastDigit (x:y:[]) = x /= lastDigit && x == y
    containsIsolatedDoubles' lastDigit (x:y:z:rest) =
      if x /= lastDigit && x == y && y /= z
        then True
        else containsIsolatedDoubles' x (y : z : rest)

possiblePasswordsAmended :: Int -> Int -> [Int]
possiblePasswordsAmended lower upper =
  filter containsIsolatedDoubles $ possiblePasswords lower upper

partTwo :: String -> Int
partTwo input = length $ possiblePasswordsAmended (read lower) (read upper)
  where
    (lower:upper:[]) = splitOn "-" input
