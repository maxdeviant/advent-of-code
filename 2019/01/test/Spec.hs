import Lib
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    it "works for sample input: 12" $ do partOne "12" `shouldBe` 2
    it "returns the correct answer for Part One" $ do
      input <- readFile "input.txt"
      partOne input `shouldBe` 3423511
    it "returns the correct answer for Part Two" $ do
      input <- readFile "input.txt"
      partTwo input `shouldBe` 5132379
