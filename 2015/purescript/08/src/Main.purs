module Main where

import Prelude
import Data.Array (drop, dropEnd)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

data EscapeSequence
  = Backslash
  | DoubleQuote
  | AsciiCharCode

newtype SantaString
  = SantaString String

derive instance newtypeSantaString :: Newtype SantaString _

countCharacters :: SantaString -> { code :: Int, inMemory :: Int }
countCharacters =
  countCharacters' { code: 2, inMemory: 0 }
    <<< List.fromFoldable
    <<< trimQuotes
    <<< toCharArray
    <<< unwrap
  where
  trimQuotes = drop 1 <<< dropEnd 1

  countCharacters' counts chars = case readEscapeSequence chars of
    Just { escapeSequence, chars: chars' } ->
      let
        charsInEscapeSequence = case escapeSequence of
          Backslash -> 2
          DoubleQuote -> 2
          AsciiCharCode -> 4
      in
        countCharacters' (counts + { code: charsInEscapeSequence, inMemory: 1 }) chars'
    Nothing -> case chars of
      Cons _ tail -> countCharacters' (counts + { code: 1, inMemory: 1 }) tail
      Nil -> counts

  readEscapeSequence chars = case chars of
    Cons head tail -> case head of
      '\\' -> case tail of
        Cons '\\' tail' -> Just { escapeSequence: Backslash, chars: tail' }
        Cons '"' tail' -> Just { escapeSequence: DoubleQuote, chars: tail' }
        Cons 'x' (Cons a (Cons b tail')) -> Just { escapeSequence: AsciiCharCode, chars: tail' }
        _ -> Nothing
      _ -> Nothing
    _ -> Nothing

partOne :: String -> Either String Int
partOne input =
  pure
    $ input
    # lines
    # map wrap
    # map countCharacters
    # map (\{ code, inMemory } -> code - inMemory)
    # sum

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
