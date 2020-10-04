module Main where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (minimum, sum)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), maybe')
import Data.String (Pattern(..), split)
import Data.String.Utils (lines)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

data Present
  = Present
    { length :: Int
    , width :: Int
    , height :: Int
    }

surfaceArea :: Present -> Int
surfaceArea (Present { length: l, width: w, height: h }) = 2 * l * w + 2 * w * h + 2 * h * l

volume :: Present -> Int
volume (Present { length, width, height }) = length * width * height

areaOfSmallestSide :: Present -> Int
areaOfSmallestSide (Present { length: l, width: w, height: h }) = maybe' (\_ -> 0) identity $ minimum [ l * w, w * h, h * l ]

perimeterOfSmallestSide :: Present -> Int
perimeterOfSmallestSide (Present { length: l, width: w, height: h }) = maybe' (\_ -> 0) identity $ minimum $ [ perimeter l w, perimeter w h, perimeter h l ]
  where
  perimeter l w = 2 * (l + w)

wrappingPaperNeeded :: Present -> Int
wrappingPaperNeeded present = surfaceArea present + areaOfSmallestSide present

parsePresent :: String -> Either String Present
parsePresent input = case map fromString $ split (Pattern "x") input of
  [ Just length, Just width, Just height ] -> Right $ Present { length, width, height }
  _ -> Left $ "Failed to parse present: " <> show input

partOne :: String -> Either String Int
partOne input = do
  presents <-
    input
      # lines
      # traverse parsePresent
  pure
    $ presents
    # map wrappingPaperNeeded
    # sum

ribbonNeeded :: Present -> Int
ribbonNeeded present = perimeterOfSmallestSide present + volume present

partTwo :: String -> Either String Int
partTwo input = do
  presents <-
    input
      # lines
      # traverse parsePresent
  pure
    $ presents
    # map ribbonNeeded
    # sum

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
