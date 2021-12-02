module Day03.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

partOne :: String -> Either String Int
partOne input = Left "Part One not implemented"

partTwo :: String -> Either String Int
partTwo input = Left "Part Two not implemented"

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "input/day03.txt"
  log "Part One"
  case partOne input of
    Right answer -> logShow answer
    Left error -> log $ "Failed with: " <> error
  log "Part Two"
  case partTwo input of
    Right answer -> logShow answer
    Left error -> log $ "Failed with: " <> error
