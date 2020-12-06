module Main where

import Prelude
import Data.Array (concatMap, uncons, (:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl, sum)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type Answers
  = Set Char

type Person
  = { answers :: Answers }

parsePerson :: String -> Person
parsePerson = toCharArray >>> Set.fromFoldable >>> { answers: _ }

type Group
  = Array Person

groupAnswers :: Group -> Answers
groupAnswers =
  foldl (flip Set.insert) Set.empty
    <<< concatMap (Array.fromFoldable <<< _.answers)

parseGroups :: String -> Array Group
parseGroups = parseGroups' [] [] <<< lines
  where
  parseGroups' groups currentGroup lines = case uncons lines of
    Just { head: "", tail } -> parseGroups' (currentGroup : groups) [] tail
    Just { head, tail } ->
      let
        person = parsePerson head
      in
        parseGroups' groups (person : currentGroup) tail
    Nothing -> Array.reverse (currentGroup : groups)

partOne :: String -> Either String Int
partOne =
  parseGroups
    >>> map groupAnswers
    >>> map Set.size
    >>> sum
    >>> pure

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
