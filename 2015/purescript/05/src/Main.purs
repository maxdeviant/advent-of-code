module Main where

import Prelude
import Data.Array (drop, filter, foldl, length, mapMaybe, reverse, uncons, zip, zipWith, (:))
import Data.Either (Either(..))
import Data.Foldable (any)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String (Pattern(..), contains)
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (lines)
import Data.Tuple (Tuple(..))
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

uncons2 :: forall a. Array a -> Maybe { x :: a, y :: a, rest :: Array a }
uncons2 arr = do
  { head: x, tail: xs } <- uncons arr
  { head: y, tail: rest } <- uncons xs
  pure $ { x, y, rest }

uncons3 :: forall a. Array a -> Maybe { x :: a, y :: a, z :: a, rest :: Array a }
uncons3 arr = do
  { head: x, tail: xs } <- uncons arr
  { head: y, tail: ys } <- uncons xs
  { head: z, tail: rest } <- uncons ys
  pure $ { x, y, z, rest }

twoCharacterPairs :: String -> Array (Tuple Char Char)
twoCharacterPairs value = zip chars (drop 1 chars)
  where
  chars = toCharArray value

twoCharacterPairsWithoutOverlaps :: String -> Array (Tuple Char Char)
twoCharacterPairsWithoutOverlaps value =
  value
    # twoCharacterPairs
    # filterOutOverlaps []
    # reverse
  where
  filterOutOverlaps acc [] = acc

  filterOutOverlaps acc [ pair ] = pair : acc

  filterOutOverlaps acc pairs = case uncons2 pairs of
    Just { x, y, rest } ->
      let
        acc' = if x == y then acc else y : x : acc
      in
        filterOutOverlaps acc' rest
    Nothing -> acc

countOccurrences :: forall a. Ord a => Array a -> Map a Int
countOccurrences xs = foldl (\acc pair -> acc # Map.insertWith (+) pair 1) Map.empty $ xs

zip3 :: forall a b c. Array a -> Array b -> Array c -> Array { x :: a, y :: b, z :: c }
zip3 u v w = zipWith (\x (Tuple y z) -> { x, y, z }) u (zip v w)

threeCharacterTuples :: String -> Array { x :: Char, y :: Char, z :: Char }
threeCharacterTuples value = zip3 chars (drop 1 chars) (drop 2 chars)
  where
  chars = toCharArray value

isRedefinedNiceString :: String -> Maybe NiceString
isRedefinedNiceString value = if meetsPairRequirements value && meetsRepeatRequirements value then Just $ wrap value else Nothing
  where
  meetsPairRequirements =
    twoCharacterPairsWithoutOverlaps
      >>> countOccurrences
      >>> Map.filter (\count -> count > 1)
      >>> (not <<< Map.isEmpty)

  meetsRepeatRequirements =
    threeCharacterTuples
      >>> filter (\{ x, z } -> x == z)
      >>> ((/=) [])

partTwo :: String -> Either String Int
partTwo input =
  pure
    $ input
    # lines
    # mapMaybe isRedefinedNiceString
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
