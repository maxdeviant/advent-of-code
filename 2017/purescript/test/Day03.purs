module Test.Day03 where

import Prelude

import Data.Either (Either(..))
import Day03.Main (partOne, partTwo)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)

day03Spec :: Spec Unit
day03Spec = do
  describe "partOne" do
    pending "returns the correct answer"

  describe "partTwo" do
    pending "returns the correct answer"
