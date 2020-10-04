module Main where

import Prelude
import Data.Array (length, mapMaybe, uncons)
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String (Pattern(..), contains)
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

newtype NiceString
  = NiceString String

derive instance newtypeNiceString :: Newtype NiceString _

instance showNiceString :: Show NiceString where
  show = show <<< unwrap

isVowel :: Char -> Boolean
isVowel 'a' = true

isVowel 'e' = true

isVowel 'i' = true

isVowel 'o' = true

isVowel 'u' = true

isVowel _ = false

countVowels :: String -> Int
countVowels = countVowels' 0 <<< toCharArray
  where
  countVowels' count chars = case uncons chars of
    Just { head, tail } -> countVowels' (if isVowel head then count + 1 else count) tail
    Nothing -> count

anyCharactersAppearTwiceInARow :: String -> Boolean
anyCharactersAppearTwiceInARow value = case uncons $ toCharArray value of
  Just { head, tail } -> anyCharactersAppearTwiceInARow' head tail
  Nothing -> false
  where
  anyCharactersAppearTwiceInARow' lastChar chars = case uncons chars of
    Just { head, tail } ->
      if head == lastChar then
        true
      else
        anyCharactersAppearTwiceInARow' head tail
    Nothing -> false

containsAny :: Array Pattern -> String -> Boolean
containsAny patterns value = any (\pattern -> value # contains pattern) patterns

isNiceString :: String -> Maybe NiceString
isNiceString value =
  if value # containsAny naughtyPatterns then
    Nothing
  else
    if countVowels value > 2 && anyCharactersAppearTwiceInARow value then Just $ wrap value else Nothing
  where
  naughtyPatterns = map Pattern [ "ab", "cd", "pq", "xy" ]

partOne :: String -> Either String Int
partOne input =
  pure
    $ input
    # lines
    # mapMaybe isNiceString
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
