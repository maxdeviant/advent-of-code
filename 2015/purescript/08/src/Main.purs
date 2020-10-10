module Main where

import Prelude
import Data.Array (foldl, snoc)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String as String
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

data EscapeSequence
  = Backslash
  | DoubleQuote
  | AsciiCharCode String

derive instance eqEscapeSequence :: Eq EscapeSequence

instance showEscapeSequence :: Show EscapeSequence where
  show Backslash = "Backslash"
  show DoubleQuote = "DoubleQuote"
  show (AsciiCharCode hexCode) = "AsciiCharCode(" <> hexCode <> ")"

data SantaStringPart
  = Quote
  | Substring String
  | EscapeSequence EscapeSequence

derive instance eqSantaStringPart :: Eq SantaStringPart

instance showSantaStringPart :: Show SantaStringPart where
  show Quote = show '"'
  show (Substring substring) = show substring
  show (EscapeSequence escapeSequence) = show escapeSequence

newtype SantaString
  = SantaString (List SantaStringPart)

derive instance newtypeSantaString :: Newtype SantaString _

derive instance eqSantaString :: Eq SantaString

instance showSantaString :: Show SantaString where
  show = show <<< unwrap

mkSantaString :: String -> SantaString
mkSantaString =
  mkSantaString' Nil []
    <<< trimQuotes
    <<< List.fromFoldable
    <<< toCharArray
  where
  trimQuotes (Cons '"' tail) = tail

  trimQuotes otherwise = otherwise

  mkSantaString' acc currentChars chars = case readEscapeSequence chars of
    Just { escapeSequence, chars: chars' } ->
      let
        acc' = case mkSubstring currentChars of
          Just substring -> Cons substring acc
          Nothing -> acc
      in
        mkSantaString' (Cons (EscapeSequence escapeSequence) acc') [] chars'
    Nothing -> case chars of
      Cons char tail -> mkSantaString' acc (snoc currentChars char) tail
      Nil ->
        let
          quote = Cons Quote >>> flip List.snoc Quote

          acc' = case mkSubstring currentChars of
            Just substring -> Cons substring acc
            Nothing -> acc
        in
          SantaString $ quote $ List.reverse acc'

  mkSubstring = case _ of
    [] -> Nothing
    chars -> Just $ Substring $ fromCharArray chars

  readEscapeSequence chars = case chars of
    Cons head tail -> case head of
      '\\' -> case tail of
        Cons '\\' tail' -> Just { escapeSequence: Backslash, chars: tail' }
        Cons '"' tail' -> Just { escapeSequence: DoubleQuote, chars: tail' }
        Cons 'x' (Cons a (Cons b tail')) -> Just { escapeSequence: AsciiCharCode $ fromCharArray [ a, b ], chars: tail' }
        _ -> Nothing
      _ -> Nothing
    _ -> Nothing

countCharacters :: SantaString -> { code :: Int, inMemory :: Int }
countCharacters =
  foldl
    ( \counts x -> case x of
        Quote -> counts + { code: 1, inMemory: 0 }
        Substring substring ->
          let
            charCount = String.length substring
          in
            counts + { code: charCount, inMemory: charCount }
        EscapeSequence escapeSequence ->
          let
            charsInEscapeSequence = case escapeSequence of
              Backslash -> 2
              DoubleQuote -> 2
              AsciiCharCode _ -> 4
          in
            counts + { code: charsInEscapeSequence, inMemory: 1 }
    )
    { code: 0, inMemory: 0 }
    <<< unwrap

partOne :: String -> Either String Int
partOne input =
  pure
    $ input
    # lines
    # map mkSantaString
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
