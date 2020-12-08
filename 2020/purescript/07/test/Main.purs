module Test.Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Main
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

leafBag :: String -> Bag
leafBag color = Bag { color: BagColor color, contains: [] }

main :: Effect Unit
main = do
  launchAff_
    $ runSpec [ consoleReporter ] do
        describe "Day 7" do
          describe "insertBag" do
            it "places a bag inside another" do
              let
                innerBag =
                  Bag
                    { color: BagColor "blue"
                    , contains:
                        [ { quantity: 1, bag: leafBag "green" }
                        , { quantity: 2, bag: leafBag "yellow" }
                        ]
                    }
              let
                outerBag =
                  Bag
                    { color: BagColor "red"
                    , contains: [ { quantity: 4, bag: leafBag "blue" } ]
                    }
              let
                expected =
                  Just
                    $ Bag
                        { color: BagColor "red"
                        , contains: [ { quantity: 4, bag: innerBag } ]
                        }
              insertBag innerBag outerBag `shouldEqual` expected
          it "places a bag inside another two levels deep" do
            let
              innerInnerBag = Bag { color: BagColor "green", contains: [ { quantity: 7, bag: leafBag "purple" } ] }
            let
              innerBag =
                Bag
                  { color: BagColor "blue"
                  , contains:
                      [ { quantity: 1, bag: leafBag "green" }
                      , { quantity: 2, bag: leafBag "yellow" }
                      ]
                  }
            let
              outerBag =
                Bag
                  { color: BagColor "red"
                  , contains: [ { quantity: 4, bag: innerBag } ]
                  }
            let
              expected =
                Just
                  $ Bag
                      { color: BagColor "red"
                      , contains:
                          [ { quantity: 4
                            , bag:
                                Bag
                                  { color: BagColor "blue"
                                  , contains:
                                      [ { quantity: 1, bag: innerInnerBag }
                                      , { quantity: 2, bag: leafBag "yellow" }
                                      ]
                                  }
                            }
                          ]
                      }
            insertBag innerInnerBag outerBag `shouldEqual` expected
