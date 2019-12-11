module Lib
  ( partOne
  ) where

import qualified Intcode as Intcode

partOne :: String -> IO ()
partOne input = do
  let (_, outputs) = Intcode.eval print $ Intcode.parse input
  sequence_ outputs
