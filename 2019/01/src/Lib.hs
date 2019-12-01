module Lib
  ( requiredFuel
  ) where

import Control.Arrow ((>>>))

requiredFuel = (/ 3) >>> floor >>> (subtract 2)
