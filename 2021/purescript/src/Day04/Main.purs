module Day04.Main where

import Prelude

import Data.Array (uncons, (!!), (..), (:))
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Foldable (sum)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, wrap)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Utils (lines)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

chunksOf :: forall a. Int -> Array a -> Array (Array a)
chunksOf n = go []
  where
  go acc [] = acc # Array.reverse
  go acc xs = go (chunk : acc) rest
    where
    { before: chunk, after: rest } = Array.splitAt n xs

rows :: Array (Array Int)
rows = (0 .. 24) # chunksOf 5

columns :: Array (Array Int)
columns = (0 .. 4) # map (go [])
  where
  go acc n | n > 24 = acc # Array.reverse
  go acc n = go (n : acc) (n + 5)

newtype BingoBoard = BingoBoard (Array { number :: Int, marked :: Boolean })

derive instance Newtype BingoBoard _

mark :: Int -> BingoBoard -> BingoBoard
mark number (BingoBoard board) = board
  # Array.findIndex (\tile -> tile.number == number)
  >>= (\index -> board # Array.modifyAt index (_ { marked = true }))
  # fromMaybe board
  # wrap

hasWon :: BingoBoard -> Boolean
hasWon (BingoBoard board) =
  [ rows, columns ]
    # Array.concat
    # Array.any (Array.mapMaybe (\index -> board !! index) >>> Array.all _.marked)

score :: BingoBoard -> Int -> Int
score (BingoBoard board) drawnNumber = board
  # Array.filter (not <<< _.marked)
  # map _.number
  # sum
  # (*) drawnNumber

parseInput :: String -> Either String { drawnNumbers :: Array Int, boards :: Array BingoBoard }
parseInput input = do
  { head: rawDrawnNumbers, tail: rawBoards } <- input # lines # uncons # note "Invalid input"

  drawnNumbers <- parseDrawnNumbers rawDrawnNumbers
  boards <- parseBoards $ Array.drop 1 $ rawBoards

  pure { drawnNumbers, boards }

  where
  parseDrawnNumbers = String.split (Pattern ",")
    >>> traverse (\n -> n # Int.fromString # note ("Invalid drawn number: " <> n))

  parseBoards = go [] []
    where
    mkBoard = map (\number -> { number, marked: false }) >>> BingoBoard

    go :: Array BingoBoard -> Array Int -> Array String -> Either String (Array BingoBoard)
    go acc board remainingLines =
      case uncons remainingLines of
        Just { head: "", tail } -> go (mkBoard board : acc) [] tail
        Just { head, tail } -> do
          parsedLine <- head
            # String.split (Pattern " ")
            # Array.filter ((/=) "")
            # traverse (\n -> n # Int.fromString # note ("Invalid board number: " <> n))
          go acc (Array.concat [ board, parsedLine ]) tail
        Nothing -> pure $ Array.reverse $ mkBoard board : acc

firstWinningBoard :: Array Int -> Array BingoBoard -> Maybe { winningBoard :: BingoBoard, drawnNumber :: Int }
firstWinningBoard drawnNumbers boards =
  case uncons drawnNumbers of
    Just { head: drawnNumber, tail } ->
      let
        boards' = boards # map (mark drawnNumber)
      in
        case boards' # Array.find hasWon of
          Just winningBoard -> Just { winningBoard, drawnNumber }
          Nothing -> firstWinningBoard tail boards'
    Nothing -> Nothing

lastWinningBoard :: Array Int -> Array BingoBoard -> Maybe { winningBoard :: BingoBoard, drawnNumber :: Int }
lastWinningBoard = go [] 0
  where
  go wonBoards lastCalledNumber _ [] =
    wonBoards
      # Array.last
      # map
          ( \lastWonBoard ->
              { winningBoard: lastWonBoard
              , drawnNumber: lastCalledNumber
              }
          )
  go wonBoards lastCalledNumber drawnNumbers boards =
    case uncons drawnNumbers of
      Just { head: drawnNumber, tail } ->
        let
          boards' = boards # map (mark drawnNumber)

          { yes: winningBoards, no: nonWinningBoards } = boards' # Array.partition hasWon
        in
          go (Array.concat [ wonBoards, winningBoards ]) drawnNumber tail nonWinningBoards
      Nothing ->
        wonBoards
          # Array.last
          # map
              ( \lastWonBoard ->
                  { winningBoard: lastWonBoard
                  , drawnNumber: lastCalledNumber
                  }
              )

partOne :: String -> Either String Int
partOne input = do
  { drawnNumbers, boards } <- parseInput input

  { winningBoard, drawnNumber } <- firstWinningBoard drawnNumbers boards # note "No winning board found!"

  pure $ score winningBoard drawnNumber

partTwo :: String -> Either String Int
partTwo input = do
  { drawnNumbers, boards } <- parseInput input

  { winningBoard, drawnNumber } <- lastWinningBoard drawnNumbers boards # note "No winning board found!"

  pure $ score winningBoard drawnNumber

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "input/day04.txt"
  log "Part One"
  case partOne input of
    Right answer -> logShow answer
    Left error -> log $ "Failed with: " <> error
  log "Part Two"
  case partTwo input of
    Right answer -> logShow answer
    Left error -> log $ "Failed with: " <> error
