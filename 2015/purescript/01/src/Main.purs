module Main where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

data Direction
  = Up
  | Down

instance showDirection :: Show Direction where
  show Up = "Up"
  show Down = "Down"

parseDirection :: Char -> Char -> Char -> Either String Direction
parseDirection upToken _ token
  | token == upToken = Right Up

parseDirection _ downToken token
  | token == downToken = Right Down

parseDirection upToken downToken token =
  Left
    $ "Failed to parse "
    <> show token
    <> " as a direction. Expected "
    <> show upToken
    <> " for "
    <> show Up
    <> " or "
    <> show downToken
    <> " for "
    <> show Down
    <> "."

partOne :: String -> Either String Int
partOne input = do
  directions <-
    input
      # toCharArray
      # traverse (parseDirection '(' ')')
  pure
    $ directions
    # map
        ( case _ of
            Up -> 1
            Down -> -1
        )
    # sum

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "input.txt"
  log "Part One"
  case partOne input of
    Right answer -> logShow answer
    Left error -> log $ "Failed with: " <> error
