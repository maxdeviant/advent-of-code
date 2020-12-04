module Main where

import Prelude
import Data.Array (concat, filter, uncons, (..))
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
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

type Location
  = { facing :: Direction, coordinates :: Point }

followInstruction :: Location -> Instruction -> Location
followInstruction { facing, coordinates } { turn, blocks } = { facing: facing', coordinates: coordinates + move }
  where
  facing' = doTurn turn facing

  move = case facing' of
    North -> { x: 0, y: blocks }
    South -> { x: 0, y: -blocks }
    East -> { x: blocks, y: 0 }
    West -> { x: -blocks, y: 0 }

followInstructions :: Array Instruction -> Point -> Point
followInstructions = followInstructions' North
  where
  followInstructions' facing instructions coordinates = case uncons instructions of
    Just { head: instruction, tail } ->
      let
        newLocation = followInstruction { facing, coordinates } instruction
      in
        followInstructions' newLocation.facing tail newLocation.coordinates
    Nothing -> coordinates

partOne :: String -> Either String Int
partOne input = do
  instructions <-
    input
      # split (Pattern ", ")
      # traverse parseInstruction
  let
    startingPoint = { x: 0, y: 0 }
  pure $ manhattanDistance startingPoint $ followInstructions instructions startingPoint

intermediateLocations :: Point -> Point -> Array Point
intermediateLocations { x: x1, y: y1 } { x: x2, y: y2 } =
  concat
    [ if deltaX > 0 then (map { x: _, y: 0 } $ 0 .. deltaX) else []
    , if deltaY > 0 then (map { x: 0, y: _ } $ 0 .. deltaY) else []
    ]
    # filter ((/=) { x: 0, y: 0 })
  where
  deltaX = abs $ x1 - x2

  deltaY = abs $ y1 - y2

firstLocationVisitedNTimes :: Int -> Array Instruction -> Point -> Maybe Point
firstLocationVisitedNTimes times instructions' coordinates =
  firstLocationVisitedNTimes'
    Map.empty
    instructions'
    { facing: North, coordinates }
  where
  visitLocations acc locations = case uncons locations of
    Just { head: location, tail } ->
      let
        acc' = acc # Map.insertWith (+) location 1

        timesVisited = acc' # Map.lookup location # fromMaybe 0
      in
        if timesVisited == times then
          Right location
        else
          visitLocations acc' tail
    Nothing -> Left acc

  firstLocationVisitedNTimes' acc instructions location = case uncons instructions of
    Just { head: instruction, tail } ->
      let
        newLocation = followInstruction location instruction

        intermediateLocations' =
          intermediateLocations location.coordinates newLocation.coordinates
            # map ((+) location.coordinates)
      in
        case visitLocations acc intermediateLocations' of
          Right found -> Just found
          Left acc' -> firstLocationVisitedNTimes' acc' tail newLocation
    Nothing -> Nothing

partTwo :: String -> Either String Int
partTwo input = do
  instructions <-
    input
      # split (Pattern ", ")
      # traverse parseInstruction
  let
    startingPoint = { x: 0, y: 0 }
  firstLocationVisitedTwice <-
    firstLocationVisitedNTimes 2 instructions startingPoint
      # note "No location was visited twice."
  pure $ manhattanDistance startingPoint firstLocationVisitedTwice

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
