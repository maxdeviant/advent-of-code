module Main where

import Prelude
import Data.Array (foldl, range)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.String.Utils (lines, words)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type Point
  = { x :: Int, y :: Int }

mkPoint :: Int -> Int -> Point
mkPoint x y = { x, y }

type LightRange
  = { start :: Point, end :: Point }

data LightStatus
  = Lit
  | Unlit

derive instance eqLightStatus :: Eq LightStatus

data LightAction
  = TurnOn
  | TurnOff
  | Toggle

type Instruction
  = { action :: LightAction, range :: LightRange }

type LightGrid
  = { size :: Int, lights :: Map Point LightStatus }

mkGrid :: Int -> LightGrid
mkGrid size =
  { size
  , lights
  }
  where
  row = range 0 (size - 1)

  points = map mkPoint row <*> row

  lights = Map.fromFoldable $ map (\point -> Tuple point Unlit) $ points

modifyLight :: LightAction -> Point -> LightGrid -> LightGrid
modifyLight action point grid =
  grid
    { lights =
      grid.lights
        # Map.update
            ( \status ->
                Just case action of
                  TurnOn -> Lit
                  TurnOff -> Unlit
                  Toggle -> case status of
                    Lit -> Unlit
                    Unlit -> Lit
            )
            point
    }

countLights :: LightStatus -> LightGrid -> Int
countLights status =
  _.lights
    >>> Map.filter ((==) status)
    >>> Map.size

pointsInRange :: LightRange -> Array Point
pointsInRange { start, end } = map mkPoint (range start.x end.y) <*> (range start.y end.y)

parsePoint :: String -> Either String Point
parsePoint text = case map Int.fromString $ split (Pattern ",") $ text of
  [ Just x, Just y ] -> Right { x, y }
  _ -> Left $ "Failed to parse point: " <> show text

parseInstruction :: String -> Either String Instruction
parseInstruction text = do
  { action, start: startText, end: endText } <- case words text of
    [ "turn", "on", start, _through, end ] -> Right { action: TurnOn, start, end }
    [ "turn", "off", start, _through, end ] -> Right { action: TurnOff, start, end }
    [ "toggle", start, _through, end ] -> Right { action: Toggle, start, end }
    _ -> Left $ "Failed to parse instruction: " <> show text
  start <- parsePoint startText
  end <- parsePoint endText
  pure $ { action, range: { start, end } }

followInstructions :: Array Instruction -> LightGrid -> LightGrid
followInstructions instructions grid =
  instructions
    # foldl (flip followInstruction) grid
  where
  followInstruction instruction grid' =
    let
      points = pointsInRange instruction.range
    in
      points
        # foldl (\acc x -> modifyLight instruction.action x acc) grid'

partOne :: String -> Either String Int
partOne input = do
  instructions <-
    input
      # lines
      # traverse parseInstruction
  let
    grid = mkGrid 1000
  let
    grid' = followInstructions instructions grid
  pure $ countLights Lit grid'

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
