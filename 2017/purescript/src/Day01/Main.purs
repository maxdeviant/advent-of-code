module Day01.Main where

import Prelude

import Data.Array ((:))
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, snoc, (!!))
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
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

solveCaptcha :: Sequence -> Int
solveCaptcha (Sequence digits') = go 0 circularDigits
  where
  firstDigit = NonEmptyArray.head digits'

  circularDigits = (NonEmptyArray.toArray $ digits' `snoc` firstDigit)

  go acc digits =
    case uncons2 digits of
      Just { x, y, rest } ->
        if x == y then
          go (acc + x) (y : rest)
        else
          go acc (y : rest)
      Nothing -> acc

data NextDigitError = OutOfBounds Int

nextDigit :: Sequence -> Int -> Either NextDigitError Int
nextDigit (Sequence digits) position
  | position == NonEmptyArray.length digits =
      Right $ NonEmptyArray.head digits
nextDigit (Sequence digits) position =
  digits !! position
    # note (OutOfBounds position)

partOne :: String -> Either String Int
partOne input = do
  sequence <- parseDigits input

  pure $ solveCaptcha sequence

partTwo :: String -> Either String Int
partTwo input = Left "Part Two not implemented."

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
