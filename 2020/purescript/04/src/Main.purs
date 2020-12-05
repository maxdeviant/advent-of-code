module Main where

import Prelude
import Data.Array (all, concat, find, length, mapMaybe, reverse, uncons, (:))
import Data.Either (Either(..), hush, note)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, splitAt)
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Data.String.Regex (regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Utils (lines)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

type RawPassportField
  = { key :: String, value :: String }

type Passport
  = { birthYear :: String
    , issueYear :: String
    , expirationYear :: String
    , height :: String
    , hairColor :: String
    , eyeColor :: String
    , passportId :: String
    , countryId :: Maybe String
    }

newtype ConstrainedYear
  = ConstrainedYear Int

mkConstrainedYear :: { atLeast :: Int, atMost :: Int } -> String -> Either String ConstrainedYear
mkConstrainedYear { atLeast, atMost } input = do
  year <- Int.fromString input # note ("Invalid year: " <> input)
  if year < atLeast || year > atMost then
    Left $ "Year out of range: " <> show year
  else
    pure $ ConstrainedYear year

newtype BirthYear
  = BirthYear ConstrainedYear

parseBirthYear :: String -> Either String BirthYear
parseBirthYear = mkConstrainedYear { atLeast: 1920, atMost: 2002 } >>> map BirthYear

newtype IssueYear
  = IssueYear ConstrainedYear

parseIssueYear :: String -> Either String IssueYear
parseIssueYear = mkConstrainedYear { atLeast: 2010, atMost: 2020 } >>> map IssueYear

newtype ExpirationYear
  = ExpirationYear ConstrainedYear

parseExpirationYear :: String -> Either String ExpirationYear
parseExpirationYear = mkConstrainedYear { atLeast: 2020, atMost: 2030 } >>> map ExpirationYear

data Height
  = Centimeters Int
  | Inches Int

parseHeight :: String -> Either String Height
parseHeight input = do
  let
    { before: value, after: units } = input # splitAt (String.length input - 2)
  value' <- value # Int.fromString # note ("Invalid height: " <> value)
  case units of
    "cm" ->
      if value' < 150 || value' > 193 then
        Left $ "Height out of range: " <> show value'
      else
        pure $ Centimeters value'
    "in" ->
      if value' < 59 || value' > 76 then
        Left $ "Height out of range: " <> show value'
      else
        pure $ Inches value'
    _ -> Left $ "Invalid unit: " <> units

newtype HairColor
  = HairColor String

parseHairColor :: String -> Either String HairColor
parseHairColor input = do
  hexRegex <- regex "#[0-9a-f]{6}$" noFlags
  if input # Regex.test hexRegex then
    pure $ HairColor input
  else
    Left $ "Invalid hair color: " <> input

data EyeColor
  = Amber
  | Blue
  | Brown
  | Gray
  | Green
  | Hazel
  | Other

parseEyeColor :: String -> Either String EyeColor
parseEyeColor = case _ of
  "amb" -> pure Amber
  "blu" -> pure Blue
  "brn" -> pure Brown
  "gry" -> pure Gray
  "grn" -> pure Green
  "hzl" -> pure Hazel
  "oth" -> pure Other
  invalidColor -> Left $ "Invalid eye color: " <> invalidColor

data PassportId
  = PassportId String

parsePassportId :: String -> Either String PassportId
parsePassportId input =
  if (input # toCharArray # all isNumber) && String.length input == 9 then
    pure $ PassportId input
  else
    Left $ "Invalid passport ID: " <> input
  where
  isNumber = case _ of
    '1' -> true
    '2' -> true
    '3' -> true
    '4' -> true
    '5' -> true
    '6' -> true
    '7' -> true
    '8' -> true
    '9' -> true
    '0' -> true
    _ -> false

type ValidatedPassport
  = { birthYear :: BirthYear
    , issueYear :: IssueYear
    , expirationYear :: ExpirationYear
    , height :: Height
    , hairColor :: HairColor
    , eyeColor :: EyeColor
    , passportId :: PassportId
    }

parsePassport :: Array RawPassportField -> Either String Passport
parsePassport fields = do
  birthYear <- findRequiredField "byr"
  issueYear <- findRequiredField "iyr"
  expirationYear <- findRequiredField "eyr"
  height <- findRequiredField "hgt"
  hairColor <- findRequiredField "hcl"
  eyeColor <- findRequiredField "ecl"
  passportId <- findRequiredField "pid"
  let
    countryId = findFieldByKey "cid" # map _.value
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

  findRequiredField key = key # findFieldByKey # note ("Missing required field: " <> key) # map _.value

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

validatePassport :: Passport -> Either String ValidatedPassport
validatePassport passport = do
  birthYear <- passport.birthYear # parseBirthYear
  issueYear <- passport.issueYear # parseIssueYear
  expirationYear <- passport.expirationYear # parseExpirationYear
  height <- passport.height # parseHeight
  hairColor <- passport.hairColor # parseHairColor
  eyeColor <- passport.eyeColor # parseEyeColor
  passportId <- passport.passportId # parsePassportId
  pure
    { birthYear
    , issueYear
    , expirationYear
    , height
    , hairColor
    , eyeColor
    , passportId
    }

partTwo :: String -> Either String Int
partTwo input = do
  passportData <- splitPassportData input
  pure
    $ passportData
    # mapMaybe (parseValidatedPassport >>> hush)
    # length
  where
  parseValidatedPassport = parsePassport >=> validatePassport

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
