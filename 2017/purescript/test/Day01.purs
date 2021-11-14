module Test.Day01 where

import Prelude

import Data.Either (Either(..))
import Day01.Main (partOne, partTwo)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

day01Spec :: Spec Unit
day01Spec = do
  describe "partOne" do
    it "returns the correct answer" do
      input <- liftEffect $ readTextFile UTF8 "input/day01.txt"

      partOne input `shouldEqual` (Right 1102)

  describe "partTwo" do
    it "returns the correct answer" do
      pure unit
