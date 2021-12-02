module Day01.Main where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, (!!))
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (foldl, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

uncons2 :: forall a. Array a -> Maybe { x :: a, y :: a, rest :: Array a }
uncons2 arr = do
  { head: x, tail: xs } <- Array.uncons arr
  { head: y, tail: rest } <- Array.uncons xs
  pure $ { x, y, rest }

newtype Sequence = Sequence (NonEmptyArray Int)

derive instance newtypeSequence :: Newtype Sequence _

parseDigits :: String -> Either String Sequence
parseDigits input =
  input
    # String.split (Pattern "")
    # traverse parseDigit
    # ((=<<) $ NonEmptyArray.fromArray >>> note "No digits")
    # map Sequence
  where
  parseDigit digit =
    digit
      # Int.fromString
      # note ("Invalid digit: " <> digit)

data SolveCaptchaError = OutOfBounds Int

instance showSolveCaptureError :: Show SolveCaptchaError where
  show (OutOfBounds position) = "Position out of bounds: " <> show position

type NextPosition = Int -> Int

solveCaptcha :: NextPosition -> Sequence -> Either SolveCaptchaError Int
solveCaptcha nextPosition (Sequence digits) = do
  pairs <-
    digits
      # traverseWithIndex
          ( \index digit -> do
              let nextIndex = nextPosition index
              nextDigit <- note (OutOfBounds nextIndex) $ digits !! nextIndex
              pure $ Tuple digit nextDigit
          )
  pure $ pairs
    # NonEmptyArray.filter (\(Tuple a b) -> a == b)
    # foldl (\acc (Tuple a _) -> acc + a) 0

partOne :: String -> Either String Int
partOne input = do
  sequence <- parseDigits input

  let
    nextDigit position =
      if position == (NonEmptyArray.length $ unwrap sequence) - 1 then 0
      else position + 1

  solveCaptcha nextDigit sequence # lmap show

partTwo :: String -> Either String Int
partTwo input = do
  sequence <- parseDigits input

  let
    sequenceLength = NonEmptyArray.length (unwrap sequence)

    step = sequenceLength / 2

    nextDigit position = (position + step) `mod` sequenceLength

  solveCaptcha nextDigit sequence # lmap show

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "input/day01.txt"
  log "Part One"
  case partOne input of
    Right answer -> logShow answer
    Left error -> log $ "Failed with: " <> error
  log "Part Two"
  case partTwo input of
    Right answer -> logShow answer
    Left error -> log $ "Failed with: " <> error
