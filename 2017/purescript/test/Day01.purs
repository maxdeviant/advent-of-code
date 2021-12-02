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
    it "works on `1212`" do
      partTwo "1212" `shouldEqual` (Right 6)

    it "works on `1221`" do
      partTwo "1221" `shouldEqual` (Right 0)

    it "works on `123425`" do
      partTwo "123425" `shouldEqual` (Right 4)

    it "works on `123123`" do
      partTwo "123123" `shouldEqual` (Right 12)

    it "works on `12131415`" do
      partTwo "12131415" `shouldEqual` (Right 4)

    it "returns the correct answer" do
      input <- liftEffect $ readTextFile UTF8 "input/day01.txt"

      partTwo input `shouldEqual` (Right 1076)
