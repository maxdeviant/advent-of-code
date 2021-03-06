module Main where

import Prelude
import Data.Array (concatMap, filter, head, length, reverse, (!!), (:))
import Data.Either (Either(..), note)
import Data.Maybe (Maybe)
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (lines)
import Data.Traversable (product, traverse)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

data Square
  = Open
  | Tree

derive instance eqSquare :: Eq Square

instance showSquare :: Show Square where
  show Open = "."
  show Tree = "#"

data Map
  = Map
    { width :: Int
    , height :: Int
    , squares :: Array Square
    }

instance showMap :: Show Map where
  show (Map theMap) = show theMap

mkMap :: String -> Either String Map
mkMap mapData = do
  let
    rows = lines mapData
  width <- rows # head # map String.length # (note "No first row")
  let
    height = length rows
  squares <- rows # concatMap toCharArray # traverse parseSquare
  pure $ Map { width, height, squares }
  where
  parseSquare = case _ of
    '.' -> Right Open
    '#' -> Right Tree
    invalidChar -> Left $ "Invalid map data: " <> show invalidChar

getSquare :: Map -> Int -> Int -> Maybe Square
getSquare (Map theMap) x y = theMap.squares !! (y * theMap.width + mod x theMap.width)

type Slope
  = { right :: Int, down :: Int }

traverseMap :: Map -> Slope -> Either String (Array Square)
traverseMap theMap@(Map { height }) { right, down } = traverseMap' { x: 0, y: 0 } [] # map reverse
  where
  traverseMap' position@{ x, y } squares =
    if y >= height then
      pure squares
    else do
      square <- getSquare theMap x y # note ("No square found at (" <> show x <> ", " <> show y <> ")")
      traverseMap' (position + { x: right, y: down }) (square : squares)

encounteredTreesCount :: Array Square -> Int
encounteredTreesCount = length <<< filter ((==) Tree)

partOne :: String -> Either String Int
partOne input = do
  theMap <- mkMap input
  traversedSquares <- traverseMap theMap { right: 3, down: 1 }
  pure $ encounteredTreesCount traversedSquares

partTwo :: String -> Either String Int
partTwo input = do
  theMap <- mkMap input
  let
    slopes =
      [ { right: 1, down: 1 }
      , { right: 3, down: 1 }
      , { right: 5, down: 1 }
      , { right: 7, down: 1 }
      , { right: 1, down: 2 }
      ]
  slopes
    # traverse (traverseMap theMap)
    # map (map encounteredTreesCount)
    # map product

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
