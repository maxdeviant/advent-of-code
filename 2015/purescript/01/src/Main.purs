module Main where

import Prelude
import Data.Array (uncons)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
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

move :: Direction -> Int
move Up = 1

move Down = -1

parseDirections :: String -> Either String (Array Direction)
parseDirections = traverse (parseDirection '(' ')') <<< toCharArray

partOne :: String -> Either String Int
partOne input = do
  directions <- parseDirections input
  pure
    $ directions
    # map move
    # sum

positionOfFirstBasementDirection :: Array Direction -> Either String Int
positionOfFirstBasementDirection = rec 0 0
  where
  rec santasPosition cursor _
    | santasPosition < 0 = Right cursor

  rec santasPosition cursor directions = case uncons directions of
    Just { head: direction, tail } -> rec (santasPosition + move direction) (cursor + 1) tail
    Nothing -> Left "Santa never entered the basement!"

partTwo :: String -> Either String Int
partTwo input = do
  directions <- parseDirections input
  positionOfFirstBasementDirection directions

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
