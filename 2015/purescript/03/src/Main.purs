module Main where

import Prelude
import Data.Array (reverse, uncons, (:))
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

data SantaOrRobotSanta a
  = Santa a
  | RobotSanta a

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

assignMoves :: Array Move -> Array (SantaOrRobotSanta Move)
assignMoves moves = reverse $ assignMoves' [] (RobotSanta unit) moves
  where
  assignMoves' acc lastMover moves' = case uncons moves' of
    Just { head: move, tail } ->
      let
        mover :: forall a. a -> SantaOrRobotSanta a
        mover = case lastMover of
          Santa unit -> RobotSanta
          RobotSanta unit -> Santa
      in
        assignMoves' (mover move : acc) (mover unit) tail
    Nothing -> acc

deliverPresentsWithRobotSanta :: Array (SantaOrRobotSanta Move) -> DeliveryReport
deliverPresentsWithRobotSanta = deliverPresents' initialLocations (Map.singleton origin 2)
  where
  origin = { x: 0, y: 0 }

  initialLocations =
    { santa: origin
    , robotSanta: origin
    }

  deliverPresents' locations report moves = case uncons moves of
    Just { head: move, tail } ->
      let
        { locations, report: report' } = case move of
          Santa santaMove ->
            deliver santaMove locations.santa report
              # \{ location, report: report' } -> { locations: locations { santa = location }, report: report' }
          RobotSanta robotSantaMove ->
            deliver robotSantaMove locations.robotSanta report
              # \{ location, report: report' } -> { locations: locations { robotSanta = location }, report: report' }
      in
        deliverPresents' locations report' tail
    Nothing -> report

  deliver move location report =
    let
      location' = runMove move location

      report' = report # Map.insertWith (+) location' 1
    in
      { location: location', report: report' }

partTwo :: String -> Either String Int
partTwo input = do
  moves <-
    input
      # toCharArray
      # traverse parseMove
  pure
    $ deliverPresentsWithRobotSanta (assignMoves moves)
    # Map.filter (\presentsDelivered -> presentsDelivered > 0)
    # Map.size

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
