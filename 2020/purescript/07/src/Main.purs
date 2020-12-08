module Main where

import Prelude
import Data.Array (any, concatMap, fold, foldl, partition, uncons, (:))
import Data.Array as Array
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
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

derive instance eqBagColor :: Eq BagColor

derive instance ordBagColor :: Ord BagColor

instance showBagColor :: Show BagColor where
  show (BagColor color) = color

type BagContents
  = { quantity :: Int, bag :: Bag }

newtype Bag
  = Bag
  { color :: BagColor
  , contains :: Array BagContents
  }

derive instance eqBag :: Eq Bag

instance showBag :: Show Bag where
  show (Bag bag) = "Bag(" <> show bag <> ")"

bagColor :: Bag -> BagColor
bagColor (Bag bag) = bag.color

placeInside :: Bag -> Bag -> Maybe Bag
placeInside innerBag@(Bag inner) (Bag outer) =
  let
    mergeContains (Bag x) (Bag y) = Bag $ x { contains = Array.concat [ x.contains, y.contains ] }

    updatedContains =
      outer.contains
        # map
            ( \contained ->
                if bagColor contained.bag == bagColor innerBag then
                  contained { bag = mergeContains contained.bag innerBag }
                else
                  contained
            )
  in
    if updatedContains == outer.contains then
      Nothing
    else
      Just $ Bag (outer { contains = updatedContains })

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

untilJust :: forall a. (a -> Maybe a) -> Array a -> Maybe (Array a)
untilJust f arr = case uncons arr of
  Just { head: x, tail: xs } -> case f x of
    Just g -> Just $ g : xs
    Nothing -> case untilJust f xs of
      Just gs -> Just $ x : gs
      Nothing -> Nothing
  Nothing -> Nothing

insertBag :: Bag -> Bag -> Maybe Bag
insertBag candidate (Bag bag) = case placeInside candidate (Bag bag) of
  Just updatedBag -> Just $ Debug.trace updatedBag \_ -> updatedBag
  Nothing -> case untilJust
      ( \contained ->
          insertBag candidate contained.bag
            # map (\updated -> contained { bag = updated })
      )
      bag.contains of
    Just updatedContains -> Just $ Bag $ bag { contains = updatedContains }
    Nothing -> Nothing

allBagColors :: Array Bag -> Set BagColor
allBagColors = map bagColor >>> Set.fromFoldable

outermostBagColors :: Array Bag -> Set BagColor
outermostBagColors bags =
  bags
    # concatMap
        ( \(Bag { contains }) ->
            contains # map (_.bag >>> bagColor)
        )
    # foldl (flip Set.delete) (allBagColors bags)

sortBags :: Array Bag -> Array Bag
sortBags bags = sortBags' outermostBags.yes outermostBags.no
  where
  outermostBags =
    let
      outermost = outermostBagColors bags
    in
      partition (\bag -> outermost # Set.member (bagColor bag)) bags

  sortBags' acc unsortedBags = case uncons unsortedBags of
    Just { head, tail } -> case untilJust (insertBag head) acc of
      Just acc' -> sortBags' acc' $ Debug.trace (show "Inserted one!") \_ -> tail
      Nothing -> sortBags' acc (Array.concat [ tail, [ head ] ])
    Nothing -> acc

partOne :: String -> Either String Int
partOne input =
  input
    # lines
    # traverse parseBag
    # map sortBags
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
