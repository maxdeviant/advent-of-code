module Lib
  ( partOne
  , partTwo
  ) where

import qualified Intcode as Intcode

partOne :: String -> IO ()
partOne input = do
  let (_, outputs) = Intcode.eval 1 print $ Intcode.parse input
  sequence_ outputs

partTwo :: String -> IO ()
partTwo input = do
  let (_, outputs) = Intcode.eval 5 print $ Intcode.parse input
  sequence_ outputs
