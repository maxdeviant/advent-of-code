module Main where

import Prelude
import Data.Array (uncons)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type Coordinates
  = { x :: Int, y :: Int }

type HouseCoordinates
  = Coordinates

type DeliveredPresents
  = Int

type DeliveryReport
  = Map HouseCoordinates DeliveredPresents

data Move
  = North
  | South
  | East
  | West

runMove :: Move -> Coordinates -> Coordinates
runMove North coordinates = coordinates { y = coordinates.y + 1 }

runMove South coordinates = coordinates { y = coordinates.y - 1 }

runMove East coordinates = coordinates { x = coordinates.x + 1 }

runMove West coordinates = coordinates { x = coordinates.x - 1 }

parseMove :: Char -> Either String Move
parseMove '^' = Right North

parseMove 'v' = Right South

parseMove '>' = Right East

parseMove '<' = Right West

parseMove illegal = Left $ "Failed to parse " <> show illegal <> " as a move."

deliverPresents :: Array Move -> DeliveryReport
deliverPresents = deliverPresents' { x: 0, y: 0 } (Map.singleton { x: 0, y: 0 } 1)
  where
  deliverPresents' location report moves = case uncons moves of
    Just { head: move, tail } ->
      let
        location' = runMove move location

        report' = report # Map.insertWith (+) location' 1
      in
        deliverPresents' location' report' tail
    Nothing -> report

partOne :: String -> Either String Int
partOne input = do
  moves <-
    input
      # toCharArray
      # traverse parseMove
  pure
    $ deliverPresents moves
    # Map.filter (\presentsDelivered -> presentsDelivered > 0)
    # Map.size

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
