module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Day01 (day01Spec)
import Test.Day02 (day02Spec)
import Test.Day03 (day03Spec)
import Test.Spec (describe)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = do
  launchAff_ $ runSpec [ consoleReporter ]
    $ describe "Advent of Code 2017" do
        describe "Day 1" day01Spec
        describe "Day 2" day02Spec
        describe "Day 3" day03Spec
