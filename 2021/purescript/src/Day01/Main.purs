module Day01.Main where

import Prelude

import Data.Array (uncons)
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String.Utils (lines)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

newtype SonarSweepReport = SonarSweepReport (Array Int)

derive instance Newtype SonarSweepReport _

parseReport :: String -> Either String SonarSweepReport
parseReport = lines >>> traverse parseLine >>> map wrap
  where
  parseLine line = line # Int.fromString # note ("Invalid depth: " <> line)

countDepthIncreases :: SonarSweepReport -> Int
countDepthIncreases = unwrap >>> go 0 Nothing
  where
  go acc lastDepth depths =
    case uncons depths of
      Just { head: depth, tail } ->
        case lastDepth of
          Nothing -> go acc (Just depth) tail
          Just lastDepth' ->
            let
              increase = if depth > lastDepth' then 1 else 0
            in
              go (acc + increase) (Just depth) tail
      Nothing -> acc

partOne :: String -> Either String Int
partOne = parseReport >>> map countDepthIncreases

partTwo :: String -> Either String Int
partTwo input = Left "Part Two not implemented."

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "input/day01.txt"
  log "Part One"
  case partOne input of
    Right answer -> logShow answer
    Left error -> log $ "Failed with: " <> error
  log "Part Two"
  case partTwo input of
    Right answer -> logShow answer
    Left error -> log $ "Failed with: " <> error
