module Day2 (partOne, partTwo) where

import Control.Arrow ((&&&))
import Control.Monad ((<=<))
import Data.List.Split
import Text.Read (readMaybe)

sumPair :: Num a => (a, a) -> a
sumPair (a, b) = a + b

type Length = Int

type Width = Int

type Height = Int

data Present = Present
  { presentLength :: Length,
    presentWidth :: Width,
    presentHeight :: Height
  }
  deriving (Show)

dimensions :: Present -> (Length, Width, Height)
dimensions present = (l, w, h)
  where
    l = presentLength present
    w = presentWidth present
    h = presentHeight present

surfaceArea :: Present -> Int
surfaceArea present = 2 * l * w + 2 * w * h + 2 * h * l
  where
    (l, w, h) = dimensions present

volume :: Present -> Int
volume present = l * w * h
  where
    (l, w, h) = dimensions present

areaOfSmallestSide :: Present -> Int
areaOfSmallestSide present = minimum [l * w, w * h, h * l]
  where
    (l, w, h) = dimensions present

wrappingPaperNeeded :: Present -> Int
wrappingPaperNeeded = sumPair . (surfaceArea &&& areaOfSmallestSide)

parsePresent :: String -> Either String Present
parsePresent input =
  case map readMaybe $ splitOn "x" input of
    [Just l, Just w, Just h] ->
      Right
        Present
          { presentLength = l,
            presentWidth = w,
            presentHeight = h
          }
    _ -> Left $ "Failed to parse present: " ++ input

partOne :: String -> Either String Int
partOne =
  fmap (sum . map wrappingPaperNeeded)
    . traverse parsePresent
    . lines

perimeterOfSmallestSide :: Present -> Int
perimeterOfSmallestSide present = minimum [perimeter l w, perimeter w h, perimeter h l]
  where
    perimeter l w = 2 * (l + w)

    (l, w, h) = dimensions present

ribbonNeeded :: Present -> Int
ribbonNeeded = sumPair . (perimeterOfSmallestSide &&& volume)

partTwo :: String -> Either String Int
partTwo =
  fmap (sum . map ribbonNeeded)
    . traverse parsePresent
    . lines
