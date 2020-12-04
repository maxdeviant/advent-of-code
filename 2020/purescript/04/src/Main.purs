module Main where

import Prelude
import Data.Array (concat, find, length, mapMaybe, reverse, uncons, (:))
import Data.Either (Either(..), hush, note)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.String.Utils (lines)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type RawPassportField
  = { key :: String, value :: String }

type Year
  = Int

data Units
  = Inches
  | Centimeters

type Height
  = String

type HexColorCode
  = String

type EyeColor
  = String

type Passport
  = { birthYear :: Year
    , issueYear :: Year
    , expirationYear :: Year
    , height :: Height
    , hairColor :: HexColorCode
    , eyeColor :: EyeColor
    , passportId :: String
    , countryId :: Maybe Int
    }

parsePassport :: Array RawPassportField -> Either String Passport
parsePassport fields = do
  birthYear <- findRequiredField "byr" >>= parseYear
  issueYear <- findRequiredField "iyr" >>= parseYear
  expirationYear <- findRequiredField "eyr" >>= parseYear
  height <- findRequiredField "hgt" # map _.value
  hairColor <- findRequiredField "hcl" # map _.value
  eyeColor <- findRequiredField "ecl" # map _.value
  passportId <- findRequiredField "pid" # map _.value
  countryId <- findFieldByKey "cid" # traverse parseCountryId
  pure
    { birthYear
    , issueYear
    , expirationYear
    , height
    , hairColor
    , eyeColor
    , passportId
    , countryId
    }
  where
  findFieldByKey key = fields # find (\field -> field.key == key)

  findRequiredField key = key # findFieldByKey # note ("Missing required field: " <> key)

  parseYear { value } =
    Int.fromString value
      # note ("Invalid year: " <> value)

  parseCountryId { value } =
    Int.fromString value
      # note ("Invalid country ID: " <> value)

splitPassportData :: String -> Either String (Array (Array RawPassportField))
splitPassportData = splitPassportData' [] [] <<< lines
  where
  splitPassportData' passports currentPassport lines = case uncons lines of
    Just { head: "", tail } -> splitPassportData' (currentPassport : passports) [] tail
    Just { head, tail } -> do
      passportFields <-
        head
          # split (Pattern " ")
          # (map $ split (Pattern ":"))
          # traverse
              ( \field -> case field of
                  [ key, value ] -> pure { key, value }
                  _ -> Left $ "Invalid passport field: " <> show field
              )
      splitPassportData' passports (concat [ currentPassport, passportFields ]) tail
    Nothing -> pure $ reverse (currentPassport : passports)

partOne :: String -> Either String Int
partOne input = do
  passportData <- splitPassportData input
  pure
    $ passportData
    # mapMaybe (parsePassport >>> hush)
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
