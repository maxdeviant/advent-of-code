module Day02.Main
  ( Command(..)
  , Coordinates
  , main
  , partOne
  , partTwo
  ) where

import Prelude

import Data.Array ((!!))
import Data.Either (Either(..), note)
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String.Utils (lines, words)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type Coordinates =
  { horizontalPosition :: Int
  , depth :: Int
  }

mkHorizontalPosition :: Int -> Coordinates
mkHorizontalPosition horizontalPosition = { horizontalPosition, depth: 0 }

mkDepth :: Int -> Coordinates
mkDepth depth = { depth, horizontalPosition: 0 }

data Command
  = Forward Int
  | Down Int
  | Up Int

parseCommand :: String -> Either String Command
parseCommand input = note ("Invalid command: " <> input) $ do
  let parts = words input
  rawDirection <- parts !! 0
  rawUnits <- parts !! 1

  mkCommand <- case rawDirection of
    "forward" -> Just Forward
    "down" -> Just Down
    "up" -> Just Up
    _ -> Nothing

  units <- Int.fromString rawUnits

  pure $ mkCommand units

executeCommands :: Coordinates -> Array Command -> Coordinates
executeCommands = foldl
  ( \acc command -> case command of
      Forward units -> acc + mkHorizontalPosition units
      Down units -> acc + mkDepth units
      Up units -> acc - mkDepth units
  )

partOne :: String -> Either String Int
partOne = lines
  >>> traverse parseCommand
  >>> map (executeCommands { horizontalPosition: 0, depth: 0 })
  >>> map (\{ horizontalPosition, depth } -> horizontalPosition * depth)

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
