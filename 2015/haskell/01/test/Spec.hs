import Day1
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    it "works with first sample input" $ do
      partOne "(())" `shouldBe` Right 0
