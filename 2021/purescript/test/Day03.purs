module Test.Day03 where

import Prelude

import Data.Either (Either(..))
import Data.Newtype (wrap)
import Data.String as String
import Day03.Main (BinaryNumber(..), Bit(..), GammaRate(..), calculateGammaRate, partOne, toDecimal)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Safe.Coerce (coerce)
import Test.Spec (Spec, describe, it, pending')
import Test.Spec.Assertions (shouldEqual)

day03Spec :: Spec Unit
day03Spec = do
  describe "toDecimal" do
    it "converts 10110 to 22" do
      toDecimal (wrap [ One, Zero, One, One, Zero ]) `shouldEqual` 22

    it "converts 01001 to 9" do
      toDecimal (wrap [ Zero, One, Zero, Zero, One ]) `shouldEqual` 9

  describe "calculateGammaRate" do
    it "works on the sample input" do
      let
        numbers =
          [ wrap [ Zero, Zero, One, Zero, Zero ]
          , wrap [ One, One, One, One, Zero ]
          , wrap [ One, Zero, One, One, Zero ]
          , wrap [ One, Zero, One, One, One ]
          , wrap [ One, Zero, One, Zero, One ]
          , wrap [ Zero, One, One, One, One ]
          , wrap [ Zero, Zero, One, One, One ]
          , wrap [ One, One, One, Zero, Zero ]
          , wrap [ One, Zero, Zero, Zero, Zero ]
          , wrap [ One, One, Zero, Zero, One ]
          , wrap [ Zero, Zero, Zero, One, Zero ]
          , wrap [ Zero, One, Zero, One, Zero ]
          ]

      calculateGammaRate numbers `shouldEqual` (coerce [ One, Zero, One, One, Zero ] :: GammaRate)

  describe "partOne" do
    it "works on the sample input" do
      let
        sampleInput =
          [ "00100"
          , "11110"
          , "10110"
          , "10111"
          , "10101"
          , "01111"
          , "00111"
          , "11100"
          , "10000"
          , "11001"
          , "00010"
          , "01010"
          ]
            # String.joinWith "\n"

      partOne sampleInput `shouldEqual` (Right 198)

    it "returns the correct answer" do
      input <- liftEffect $ readTextFile UTF8 "input/day03.txt"

      partOne input `shouldEqual` (Right 2972336)

  describe "partTwo" do
    pending' "returns the correct answer" do
      input <- liftEffect $ readTextFile UTF8 "input/day03.txt"

      pure unit
