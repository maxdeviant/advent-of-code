module Day02.Main where

import Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.Array.NonEmpty.Internal (NonEmptyArray)
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.Semigroup.Foldable (maximum, minimum)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Utils (lines)
import Data.Traversable (sum, traverse)
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
    String.split (Pattern "\t")
      >>> traverse (\column -> column # Int.fromString # note ("Invalid column data: " <> column))
      >=> (NonEmptyArray.fromArray >>> note "Row is empty")

computeChecksum :: Spreadsheet -> Int
computeChecksum (Spreadsheet rows) =
  rows
    # map (\row -> maximum row - minimum row)
    # sum

partOne :: String -> Either String Int
partOne input = do
  spreadsheet <- parseSpreadsheet input

  pure $ computeChecksum spreadsheet

partTwo :: String -> Either String Int
partTwo input = Left "Part Two not implemented."

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
