module Day01.Main where

import Prelude

import Data.Array (snoc, uncons)
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String.Utils (lines)
import Data.Traversable (sum, traverse)
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

newtype SlidingWindow = SlidingWindow (Array Int)

derive instance Newtype SlidingWindow _

derive newtype instance Eq SlidingWindow
derive newtype instance Show SlidingWindow

mkSlidingWindows :: { size :: Int } -> SonarSweepReport -> Array SlidingWindow
mkSlidingWindows { size } = unwrap >>> go [] [ [] ]
  where
  go :: Array SlidingWindow -> Array (Array Int) -> Array Int -> Array SlidingWindow
  go acc openWindows depths =
    case uncons depths of
      Just { head, tail } ->
        let
          { yes: closedWindows, no: openWindows' } =
            openWindows
              # map (flip snoc $ head)
              # (<>) [ [] ]
              # Array.partition (Array.length >>> (_ == size))
        in
          go (acc <> map wrap closedWindows) openWindows' tail
      Nothing -> acc

sumWindow :: SlidingWindow -> Int
sumWindow = unwrap >>> sum

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

countDepthIncreasesInSlidingWindows :: { size :: Int } -> SonarSweepReport -> Int
countDepthIncreasesInSlidingWindows options = mkSlidingWindows options >>> go 0 Nothing
  where
  go acc lastSum windows =
    case uncons windows of
      Just { head: window, tail } ->
        let
          windowSum = sumWindow window
        in
          case lastSum of
            Nothing -> go acc (Just windowSum) tail
            Just lastSum' ->
              let
                increase = if windowSum > lastSum' then 1 else 0
              in
                go (acc + increase) (Just windowSum) tail
      Nothing -> acc

partOne :: String -> Either String Int
partOne = parseReport >>> map countDepthIncreases

partTwo :: String -> Either String Int
partTwo = parseReport >>> map (countDepthIncreasesInSlidingWindows { size: 3 })

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
