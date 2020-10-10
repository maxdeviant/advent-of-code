module Test.Main where

import Prelude
import Data.List (List(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Main (SantaString(..), countCharacters, mkSantaString)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = do
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Day 8" do
          describe "mkSantaString" do
            it "works with an empty string" do
              let
                expected = SantaString Nil
              let
                actual = mkSantaString ""
              expected `shouldEqual` actual
          describe "countCharacters" do
            let
              test value expected =
                it ("counts the characters in \"" <> value <> "\" correctly") do
                  let
                    actual = countCharacters $ mkSantaString value
                  actual `shouldEqual` expected
            test "" { code: 2, inMemory: 0 }
            test "abc" { code: 5, inMemory: 3 }
            test "aaa\\\"aaa" { code: 10, inMemory: 7 }
            test "\\x27" { code: 6, inMemory: 1 }
