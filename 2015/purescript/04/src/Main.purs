module Main where

import Prelude
import Data.Either (Either(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.String.Utils (startsWith)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Crypto.Hash (Algorithm(..))
import Node.Crypto.Hash as Hash
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

newtype SecretKey
  = SecretKey String

derive instance newtypeSecretKey :: Newtype SecretKey _

newtype Md5Hash
  = Md5Hash String

derive instance newtypeMd5Hash :: Newtype Md5Hash _

md5 :: String -> Effect Md5Hash
md5 = map wrap <<< Hash.hex MD5

computeHash :: SecretKey -> Int -> Effect Md5Hash
computeHash (SecretKey secretKey) n = md5 $ secretKey <> show n

findNumberProducingHashWithPrefix :: String -> SecretKey -> Effect Int
findNumberProducingHashWithPrefix prefix secretKey = findHash' 1
  where
  findHash' n = do
    hash <- computeHash secretKey n
    if hash # unwrap # startsWith prefix then
      pure n
    else
      findHash' (n + 1)

partOne :: String -> Effect (Either String Int)
partOne input = do
  let
    secretKey = wrap input
  n <- map pure $ findNumberProducingHashWithPrefix "00000" secretKey
  pure n

partTwo :: String -> Either String Int
partTwo input = Left "Part Two not implemented."

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "input.txt"
  log "Part One"
  partOneResult <- partOne input
  case partOneResult of
    Right answer -> logShow answer
    Left error -> log $ "Failed with: " <> error
  log "Part Two"
  case partTwo input of
    Right answer -> logShow answer
    Left error -> log $ "Failed with: " <> error
