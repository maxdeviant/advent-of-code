import Day1
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    it "works" $ do
      2 `shouldBe` 2