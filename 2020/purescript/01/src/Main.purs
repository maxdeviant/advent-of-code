module Main where

import Prelude
import Data.Array (filter, head)
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.String.Utils (lines)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

parseExpenseReport :: String -> Either String (Array Int)
parseExpenseReport =
  lines
    >>> traverse
        ( \line ->
            line # Int.fromString # note ("Failed to parse " <> line)
        )

partOne :: String -> Either String Int
partOne input = do
  let
    sumToFind = 2020
  expenseReport <- parseExpenseReport input
  [ Tuple ] <*> expenseReport <*> expenseReport
    # filter (\(Tuple x y) -> x + y == sumToFind)
    # map (\(Tuple x y) -> x * y)
    # head
    # (note $ "No entries found that sum to " <> show sumToFind)

partTwo :: String -> Either String Int
partTwo input = Left "Part Two not implemented."

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "input.txt"
  log "Part One"
  case partOne input of
    Right answer -> logShow answer
    Left error -> log $ "Failed with: " <> error
  log "Part Two"
  case partTwo input of
    Right answer -> logShow answer
    Left error -> log $ "Failed with: " <> error
