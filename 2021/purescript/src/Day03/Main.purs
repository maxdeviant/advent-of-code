module Day03.Main where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int (pow)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, over2, unwrap, wrap)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.Utils (lines)
import Data.Traversable (sum, traverse)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Safe.Coerce (coerce)

data Bit
  = Zero
  | One

derive instance Eq Bit
derive instance Ord Bit

instance Show Bit where
  show Zero = "0"
  show One = "1"

parseBit :: String -> Either String Bit
parseBit "0" = Right Zero
parseBit "1" = Right One
parseBit input = Left $ "Invalid bit: " <> input

newtype BinaryNumber = BinaryNumber (Array Bit)

derive instance Newtype BinaryNumber _

derive newtype instance Eq BinaryNumber

instance Show BinaryNumber where
  show = unwrap >>> map show >>> String.joinWith ""

parseBinaryNumber :: String -> Either String BinaryNumber
parseBinaryNumber = String.split (Pattern "") >>> traverse parseBit >>> map wrap

toDecimal :: BinaryNumber -> Int
toDecimal = unwrap >>> Array.reverse
  >>> map
    ( case _ of
        Zero -> 0
        One -> 1
    )
  >>> Array.mapWithIndex (\n d -> d * 2 `pow` n)
  >>> sum

newtype GammaRate = GammaRate BinaryNumber

derive instance Newtype GammaRate _

derive newtype instance Eq GammaRate
derive newtype instance Show GammaRate

newtype EpsilonRate = EpsilonRate BinaryNumber

derive instance Newtype EpsilonRate _

derive newtype instance Eq EpsilonRate
derive newtype instance Show EpsilonRate

newtype OxygenGeneratorRating = OxygenGeneratorRating BinaryNumber

derive instance Newtype OxygenGeneratorRating _

derive newtype instance Eq OxygenGeneratorRating
derive newtype instance Show OxygenGeneratorRating

newtype Co2ScrubberRating = Co2ScrubberRating BinaryNumber

derive instance Newtype Co2ScrubberRating _

derive newtype instance Eq Co2ScrubberRating
derive newtype instance Show Co2ScrubberRating

newtype BitOccurrences = BitOccurrences
  { zero :: Additive Int
  , one :: Additive Int
  }

derive instance Newtype BitOccurrences _

derive newtype instance Semigroup BitOccurrences
derive newtype instance Monoid BitOccurrences

instance Semiring BitOccurrences where
  add (BitOccurrences a) (BitOccurrences b) = wrap
    { zero: a.zero `over2 Additive (+)` b.zero
    , one: a.one `over2 Additive (+)` b.one
    }
  zero = wrap { zero: wrap zero, one: wrap zero }
  mul (BitOccurrences a) (BitOccurrences b) = wrap
    { zero: a.zero `over2 Additive (*)` b.zero
    , one: a.one `over2 Additive (*)` b.one
    }
  one = wrap { zero: wrap one, one: wrap one }

getOccurrences :: Bit -> BitOccurrences -> Int
getOccurrences Zero = unwrap >>> _.zero >>> unwrap
getOccurrences One = unwrap >>> _.one >>> unwrap

countOccurrences :: Array BinaryNumber -> Map Int BitOccurrences
countOccurrences = foldl (\acc number -> acc # Map.unionWith (+) (countBitOccurrences number)) Map.empty
  where
  countBitOccurrences :: BinaryNumber -> Map Int BitOccurrences
  countBitOccurrences = unwrap >>> foldlWithIndex
    ( \index acc bit -> acc
        # Map.unionWith (+)
            ( Map.singleton index
                ( case bit of
                    Zero -> wrap { zero: wrap one, one: wrap zero }
                    One -> wrap { zero: wrap zero, one: wrap one }
                )
            )
    )
    Map.empty

calculateGammaRate :: Array BinaryNumber -> GammaRate
calculateGammaRate = countOccurrences
  >>> Map.values
  >>> map (\occurrences -> if getOccurrences Zero occurrences > getOccurrences One occurrences then Zero else One)
  >>> Array.fromFoldable
  >>> coerce

calculateEpsilonRate :: Array BinaryNumber -> EpsilonRate
calculateEpsilonRate = countOccurrences
  >>> Map.values
  >>> map (\occurrences -> if getOccurrences Zero occurrences > getOccurrences One occurrences then One else Zero)
  >>> Array.fromFoldable
  >>> coerce

calculatePowerConsumption :: GammaRate -> EpsilonRate -> Int
calculatePowerConsumption gammaRate epsilonRate =
  (toDecimal $ unwrap gammaRate) * (toDecimal $ unwrap epsilonRate)

calculateOxygenGeneratorRating :: Array BinaryNumber -> OxygenGeneratorRating
calculateOxygenGeneratorRating = go 0
  where
  go position =
    case _ of
      [ number ] -> wrap number
      numbers ->
        let
          { yes: ones, no: zeros } = numbers # Array.partition (\number -> unwrap number !! position == Just One)

          keep = if Array.length ones >= Array.length zeros then ones else zeros
        in
          go (position + 1) keep

calculateCo2ScrubberRating :: Array BinaryNumber -> Co2ScrubberRating
calculateCo2ScrubberRating = go 0
  where
  go position =
    case _ of
      [ number ] -> wrap number
      numbers ->
        let
          { yes: ones, no: zeros } = numbers # Array.partition (\number -> unwrap number !! position == Just One)

          keep = if Array.length ones < Array.length zeros then ones else zeros
        in
          go (position + 1) keep

calculateLifeSupportRating :: OxygenGeneratorRating -> Co2ScrubberRating -> Int
calculateLifeSupportRating oxygenGeneratorRating co2ScrubberRating =
  (toDecimal $ unwrap oxygenGeneratorRating) * (toDecimal $ unwrap co2ScrubberRating)

partOne :: String -> Either String Int
partOne input = do
  numbers <- input # lines # traverse parseBinaryNumber

  let gammaRate = calculateGammaRate numbers
  let epsilonRate = calculateEpsilonRate numbers

  pure $ calculatePowerConsumption gammaRate epsilonRate

partTwo :: String -> Either String Int
partTwo input = do
  numbers <- input # lines # traverse parseBinaryNumber

  let oxygenGeneratorRating = calculateOxygenGeneratorRating numbers
  let co2ScrubberRating = calculateCo2ScrubberRating numbers

  pure $ calculateLifeSupportRating oxygenGeneratorRating co2ScrubberRating

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "input/day03.txt"
  log "Part One"
  case partOne input of
    Right answer -> logShow answer
    Left error -> log $ "Failed with: " <> error
  log "Part Two"
  case partTwo input of
    Right answer -> logShow answer
    Left error -> log $ "Failed with: " <> error
