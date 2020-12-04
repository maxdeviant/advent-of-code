module Main where

import Prelude
import Data.Array (uncons)
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.String (Pattern(..), split, splitAt)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type Point
  = { x :: Int, y :: Int }

manhattanDistance :: Point -> Point -> Int
manhattanDistance { x: a, y: b } { x: c, y: d } = abs (a - c) + abs (b - d)

data Direction
  = North
  | South
  | East
  | West

data Turn
  = TurnLeft
  | TurnRight

doTurn :: Turn -> Direction -> Direction
doTurn TurnLeft North = West

doTurn TurnLeft South = East

doTurn TurnLeft East = North

doTurn TurnLeft West = South

doTurn TurnRight North = East

doTurn TurnRight South = West

doTurn TurnRight East = South

doTurn TurnRight West = North

type Instruction
  = { turn :: Turn, blocks :: Int }

parseInstruction :: String -> Either String Instruction
parseInstruction input = case splitAt 1 input of
  { before: turn', after: blocks' } -> do
    turn <- case turn' of
      "L" -> pure TurnLeft
      "R" -> pure TurnRight
      invalidTurn -> Left $ "Invalid turn: " <> invalidTurn
    blocks <- blocks' # Int.fromString # note ("Failed to parse blocks: " <> blocks')
    pure $ { turn, blocks }

followInstructions :: Array Instruction -> Point -> Point
followInstructions = followInstructions' North
  where
  followInstructions' facing instructions position = case uncons instructions of
    Just { head: { turn, blocks }, tail } ->
      let
        facing' = doTurn turn facing

        move = case facing' of
          North -> { x: 0, y: blocks }
          South -> { x: 0, y: -blocks }
          East -> { x: blocks, y: 0 }
          West -> { x: -blocks, y: 0 }
      in
        followInstructions' facing' tail $ position + move
    Nothing -> position

partOne :: String -> Either String Int
partOne input = do
  instructions <-
    input
      # split (Pattern ", ")
      # traverse parseInstruction
  let
    startingPoint = { x: 0, y: 0 }
  pure $ manhattanDistance startingPoint $ followInstructions instructions startingPoint

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
