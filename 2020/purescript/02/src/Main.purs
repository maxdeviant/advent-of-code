module Main where

import Prelude
import Data.Array (filter, length, uncons)
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, trim)
import Data.String.CodeUnits (toChar, toCharArray)
import Data.String.Utils (lines)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type Password
  = String

type PasswordPolicy
  = { letter :: Char
    , atLeast :: Int
    , atMost :: Int
    }

parseEntry :: String -> Either String { policy :: PasswordPolicy, password :: Password }
parseEntry entry = case split (Pattern ":") entry of
  [ policy, password ] -> case split (Pattern " ") policy of
    [ occurrences, letter' ] -> do
      letter <- toChar letter' # note ("Failed to parse letter: " <> letter')
      case split (Pattern "-") occurrences of
        [ atLeast', atMost' ] -> do
          atLeast <- Int.fromString atLeast' # note ("Failed to parse \"at least\" constraint: " <> atLeast')
          atMost <- Int.fromString atMost' # note ("Failed to parse \"at most\" constraint: " <> atMost')
          pure { policy: { letter: letter, atLeast, atMost }, password: trim password }
        _ -> Left $ "Invalid entry: " <> entry
    _ -> Left $ "Invalid entry: " <> entry
  _ -> Left $ "Invalid entry: " <> entry

isPasswordValid :: PasswordPolicy -> Password -> Boolean
isPasswordValid policy password = policy.atLeast <= occurrences && occurrences <= policy.atMost
  where
  occurrences = countOccurrences policy.letter 0 (toCharArray password)

  countOccurrences letter acc chars = case uncons chars of
    Just { head, tail } -> countOccurrences letter (if head == letter then acc + 1 else acc) tail
    Nothing -> acc

partOne :: String -> Either String Int
partOne input = do
  passwords <- input # lines # traverse parseEntry
  pure
    $ passwords
    # filter (\{ policy, password } -> isPasswordValid policy password)
    # length

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
