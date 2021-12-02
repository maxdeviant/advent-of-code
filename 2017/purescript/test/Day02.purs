module Test.Day02 where

import Prelude

import Data.Either (Either(..))
import Day02.Main (partOne, partTwo)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

day02Spec :: Spec Unit
day02Spec = do
  describe "partOne" do
    it "returns the correct answer" do
      input <- liftEffect $ readTextFile UTF8 "input/day02.txt"

      partOne input `shouldEqual` (Right 34581)

  describe "partTwo" do
    it "works with the sample input" do
      let
        input = "5 9 2 8\n9 4 7 3\n3 8 6 5"

      partTwo input `shouldEqual` (Right 9)

    it "returns the correct answer" do
      input <- liftEffect $ readTextFile UTF8 "input/day02.txt"

      partTwo input `shouldEqual` (Right 214)
