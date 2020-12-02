module Main where

import Prelude
import Data.Array (filter, length, uncons)
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, trim)
import Data.String.CodeUnits (charAt, toChar, toCharArray)
import Data.String.Utils (lines)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type Password
  = String

type SledRentalPasswordPolicy
  = { letter :: Char
    , x :: Int
    , y :: Int
    }

mkSledRentalPasswordPolicy :: Char -> Int -> Int -> SledRentalPasswordPolicy
mkSledRentalPasswordPolicy letter x y = { letter, x, y }

type OfficialTobogganCorporatePolicy
  = { letter :: Char
    , firstPosition :: Int
    , secondPosition :: Int
    }

mkOfficialTobogganCorporatePolicy :: Char -> Int -> Int -> OfficialTobogganCorporatePolicy
mkOfficialTobogganCorporatePolicy letter firstPosition secondPosition = { letter, firstPosition, secondPosition }

type MakePasswordPolicy policy
  = Char -> Int -> Int -> policy

parseEntry :: forall policy. MakePasswordPolicy policy -> String -> Either String { policy :: policy, password :: Password }
parseEntry mkPolicy entry = case split (Pattern ":") entry of
  [ policy, password ] -> case split (Pattern " ") policy of
    [ occurrences, letter' ] -> do
      letter <- toChar letter' # note ("Failed to parse letter: " <> letter')
      case split (Pattern "-") occurrences of
        [ x', y' ] -> do
          x <- Int.fromString x' # note ("Failed to parse first number: " <> x')
          y <- Int.fromString y' # note ("Failed to parse second number: " <> y')
          pure { policy: mkPolicy letter x y, password: trim password }
        _ -> Left $ "Invalid entry: " <> entry
    _ -> Left $ "Invalid entry: " <> entry
  _ -> Left $ "Invalid entry: " <> entry

isSledRentalPasswordValid :: SledRentalPasswordPolicy -> Password -> Boolean
isSledRentalPasswordValid policy password = policy.x <= occurrences && occurrences <= policy.y
  where
  occurrences = countOccurrences policy.letter 0 (toCharArray password)

  countOccurrences letter acc chars = case uncons chars of
    Just { head, tail } -> countOccurrences letter (if head == letter then acc + 1 else acc) tail
    Nothing -> acc

partOne :: String -> Either String Int
partOne input = do
  passwords <- input # lines # traverse (parseEntry mkSledRentalPasswordPolicy)
  pure
    $ passwords
    # filter (\{ policy, password } -> isSledRentalPasswordValid policy password)
    # length

isOfficialTobogganCorporatePasswordValid :: OfficialTobogganCorporatePolicy -> Password -> Boolean
isOfficialTobogganCorporatePasswordValid policy password = case Tuple hasLetterInFirstPosition hasLetterInSecondPosition of
  Tuple true false -> true
  Tuple false true -> true
  Tuple true true -> false
  Tuple false false -> false
  where
  hasLetterInFirstPosition = charAt (policy.firstPosition - 1) password == Just policy.letter

  hasLetterInSecondPosition = charAt (policy.secondPosition - 1) password == Just policy.letter

partTwo :: String -> Either String Int
partTwo input = do
  passwords <- input # lines # traverse (parseEntry mkOfficialTobogganCorporatePolicy)
  pure
    $ passwords
    # filter (\{ policy, password } -> isOfficialTobogganCorporatePasswordValid policy password)
    # length

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
