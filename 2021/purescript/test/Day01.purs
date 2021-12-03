module Test.Day01 where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (wrap)
import Day01.Main (mkSlidingWindows, partOne, partTwo)
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

      partOne input `shouldEqual` (Right 1665)

  describe "partTwo" do
    it "returns the correct answer" do
      input <- liftEffect $ readTextFile UTF8 "input/day01.txt"

      partTwo input `shouldEqual` (Right 1702)

  describe "mkSlidingWindows" do
    it "works with the sample data" do
      let
        report = wrap
          [ 199
          , 200
          , 208
          , 210
          , 200
          , 207
          , 240
          , 269
          , 260
          , 263
          ]
        expected =
          [ wrap
              [ 199
              , 200
              , 208
              ]
          , wrap
              [ 200
              , 208
              , 210
              ]
          , wrap
              [ 208
              , 210
              , 200
              ]
          , wrap
              [ 210
              , 200
              , 207
              ]
          , wrap
              [ 200
              , 207
              , 240
              ]
          , wrap
              [ 207
              , 240
              , 269
              ]
          , wrap
              [ 240
              , 269
              , 260
              ]
          , wrap
              [ 269
              , 260
              , 263
              ]
          ]

      mkSlidingWindows { size: 3 } report `shouldEqual` expected
