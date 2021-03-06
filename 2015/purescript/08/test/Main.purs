module Test.Main where

import Prelude
import Data.List (List(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Main
  ( EscapeSequence(..)
  , SantaString(..)
  , SantaStringPart(..)
  , countCharacters
  , countEncodedCharacters
  , mkSantaString
  )
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
            let
              test value expected =
                it ("works with \"" <> value <> "\"") do
                  let
                    actual = mkSantaString value
                  actual `shouldEqual` expected
            test "" $ SantaString $ Cons Quote $ Cons Quote Nil
            test "abc" $ SantaString $ Cons Quote $ Cons (Substring "abc") $ Cons Quote Nil
            test "aaa\\\"aaa" $ SantaString $ Cons Quote $ Cons (Substring "aaa") $ Cons (EscapeSequence DoubleQuote) $ Cons (Substring "aaa") $ Cons Quote Nil
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
          describe "countEncodedCharacters" do
            let
              test value expected =
                it ("counts the characters in \"" <> value <> "\" correctly") do
                  let
                    actual = countEncodedCharacters value
                  actual `shouldEqual` expected
            test "" 6
            test "abc" 9
            test "aaa\\\"aaa" 16
            test "\\x27" 11
