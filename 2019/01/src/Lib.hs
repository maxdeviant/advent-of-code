module Lib
  ( partOne
  ) where

import Control.Arrow ((>>>))

requiredFuel = (/ 3) >>> floor >>> (subtract 2)

partOne = lines >>> map read >>> map requiredFuel >>> foldr (+) 0
