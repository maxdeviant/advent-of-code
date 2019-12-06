module Lib
  ( partOne
  ) where

import qualified Data.List as List
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)

type Point = (Int, Int)

addPoints :: Point -> Point -> Point
addPoints (a, b) (c, d) = (a + c, b + d)

data Line =
  Line Point Point
  deriving (Show)

data Move
  = MoveUp Int
  | MoveDown Int
  | MoveLeft Int
  | MoveRight Int

type Path = [Line]

manhattanDistance :: Point -> Point -> Int
manhattanDistance (a, b) (c, d) = abs (a - c) + abs (b - d)

parseMove :: String -> Move
parseMove (direction:tail) =
  case direction of
    'U' -> MoveUp n
    'D' -> MoveDown n
    'L' -> MoveLeft n
    'R' -> MoveRight n
  where
    n = read tail

plotPath :: [Move] -> Path
plotPath moves = plotPath' [] (0, 0) moves
  where
    plotPath' path _ [] = path
    plotPath' path position (move:moves) =
      let change =
            case move of
              MoveUp yDelta -> (0, yDelta)
              MoveDown yDelta -> (0, -yDelta)
              MoveLeft xDelta -> (-xDelta, 0)
              MoveRight xDelta -> (xDelta, 0)
          endPosition = position `addPoints` change
       in plotPath' (Line position endPosition : path) endPosition moves

expand :: Line -> [Point]
expand (Line (x1, y1) (x2, y2)) =
  [ (x, y)
  | x <-
      if x1 == x2
        then [x1]
        else [x1,x1 + (signum $ x2 - x1) .. x2]
  , y <-
      if y1 == y2
        then [y1]
        else [y1,y1 + (signum $ y2 - y1) .. y2]
  ]

intersectionPoints :: Line -> Line -> [Point]
intersectionPoints lineOne lineTwo =
  List.intersect (expand lineOne) (expand lineTwo)

findIntersections :: Path -> Path -> [Point]
findIntersections pathOne pathTwo =
  let allLinePairs = [(a, b) | a <- pathOne, b <- pathTwo]
   in concatMap (\(a, b) -> intersectionPoints a b) allLinePairs

partOne :: String -> Int
partOne input =
  let (wireOne:wireTwo:[]) = lines input
      parseMoves = map parseMove . splitOn ","
      pathOne = plotPath . parseMoves $ wireOne
      pathTwo = plotPath . parseMoves $ wireTwo
      intersections = filter (/= (0, 0)) $ findIntersections pathOne pathTwo
   in foldr1 min $ map (manhattanDistance (0, 0)) intersections
