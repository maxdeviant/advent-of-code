module Lib
  ( partOne
  , partTwo
  ) where

import Control.Arrow ((>>>))

requiredFuel :: Double -> Int
requiredFuel = (/ 3) >>> floor >>> (subtract 2)

partOne :: String -> Int
partOne = lines >>> map read >>> map requiredFuel >>> foldr (+) 0

actualRequiredFuel :: Double -> Int
actualRequiredFuel = actualRequiredFuel' 0
  where
    actualRequiredFuel' acc mass =
      let fuel = requiredFuel mass
       in if fuel <= 0
            then acc
            else actualRequiredFuel' (acc + fuel) $ fromIntegral fuel

partTwo :: String -> Int
partTwo = lines >>> map read >>> map actualRequiredFuel >>> foldr (+) 0
