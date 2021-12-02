module Day02.Main where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Semigroup.Foldable (maximum, minimum)
import Data.String.Utils (lines, words)
import Data.Traversable (sum, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

newtype Spreadsheet = Spreadsheet (Array (NonEmptyArray Int))

parseSpreadsheet :: String -> Either String Spreadsheet
parseSpreadsheet =
  lines
    >>> traverse parseRow
    >>> map Spreadsheet
  where
  parseRow =
    words
      >>> traverse (\column -> column # Int.fromString # note ("Invalid column data: " <> column))
      >=> (NonEmptyArray.fromArray >>> note "Row is empty")

computeChecksum :: Spreadsheet -> Int
computeChecksum (Spreadsheet rows) =
  rows
    # map (\row -> maximum row - minimum row)
    # sum

sumEvenlyDivisibleValues :: Spreadsheet -> Either String Int
sumEvenlyDivisibleValues (Spreadsheet rows) =
  rows
    # traverse evenlyDivisibleValues
    # map sum
  where
  evenlyDivisibleValues row =
    row'
      # Array.concatMap
          ( \x -> row' # Array.mapMaybe
              ( \y ->
                  if x /= y then Just $ Tuple x y else Nothing
              )
          )
      # Array.mapMaybe (\(Tuple x y) -> x / y # Int.fromNumber)
      # Array.head
      # note "Two evenly divisible numbers not found in row."
    where
    row' = row # map Int.toNumber # NonEmptyArray.toArray

partOne :: String -> Either String Int
partOne input = do
  spreadsheet <- parseSpreadsheet input

  pure $ computeChecksum spreadsheet

partTwo :: String -> Either String Int
partTwo input = do
  spreadsheet <- parseSpreadsheet input

  sumEvenlyDivisibleValues spreadsheet

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "input/day02.txt"
  log "Part One"
  case partOne input of
    Right answer -> logShow answer
    Left error -> log $ "Failed with: " <> error
  log "Part Two"
  case partTwo input of
    Right answer -> logShow answer
    Left error -> log $ "Failed with: " <> error
