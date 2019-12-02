import Lib
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    it "returns the correct answer for Part One" $ do
      input <- readFile "input.txt"
      partOne input `shouldBe` 7210630
    it "returns the correct answer for Part Two" $ do
      input <- readFile "input.txt"
      partTwo input `shouldBe` 3892
