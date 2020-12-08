module Main where

import Prelude
import Data.Array (uncons)
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll, split, trim)
import Data.String.Utils (lines)
import Data.Traversable (traverse)
import Debug.Trace as Debug
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

newtype BagColor
  = BagColor String

type BagContents
  = { quantity :: Int, bag :: Bag }

newtype Bag
  = Bag
  { color :: BagColor
  , contains :: Array BagContents
  }

parseBag :: String -> Either String Bag
parseBag rule = case rule # split (Pattern "contain") # map stripFluff # map trim of
  [ colorText, ruleText ] -> do
    contains <- ruleText # parseContains
    pure $ Bag { color: BagColor colorText, contains }
  _ -> Left $ "Invalid rule: " <> rule
  where
  stripBagWords =
    replaceAll (Pattern "bags") (Replacement "")
      >>> replaceAll (Pattern "bag") (Replacement "")

  stripFluff =
    stripBagWords
      >>> replaceAll (Pattern ".") (Replacement "")
      >>> trim

  parseRule rule = case uncons $ split (Pattern " ") rule of
    Just { head, tail } -> do
      quantity <- head # Int.fromString # note ("Failed to parse quantity: " <> rule)
      let
        bag = Bag { color: BagColor $ joinWith " " tail, contains: [] }
      pure { quantity, bag }
    Nothing -> Left $ "Invalid rule: " <> rule

  parseContains "no other" = pure []

  parseContains input =
    input
      # split (Pattern ",")
      # map trim
      # traverse parseRule

partOne :: String -> Either String Int
partOne input =
  input
    # lines
    # traverse parseBag
    # \bag ->
        Debug.trace bag \_ ->
          bag
            # \_ -> pure 1

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
