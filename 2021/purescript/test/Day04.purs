module Test.Day04 where

import Prelude

import Data.Either (Either(..))
import Day04.Main (partOne, partTwo)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Test.Spec (Spec, describe, it, pending')
import Test.Spec.Assertions (shouldEqual)

day04Spec :: Spec Unit
day04Spec = do
  describe "partOne" do
    it "returns the correct answer" do
      input <- liftEffect $ readTextFile UTF8 "input/day04.txt"

      partOne input `shouldEqual` (Right 28082)

  describe "partTwo" do
    pending' "returns the correct answer" do
      input <- liftEffect $ readTextFile UTF8 "input/day04.txt"

      partTwo input `shouldEqual` (Right 0)
