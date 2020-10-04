import Lib
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    it "works for first sample input for Part One" $ do
      partOne "R8,U5,L5,D3\nU7,R6,D4,L4" `shouldBe` 6
    it "works for second sample input for Part One" $ do
      partOne
        "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83" `shouldBe`
        159
    it "works for third sample input for Part One" $ do
      partOne
        "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7" `shouldBe`
        135
    it "works for first sample input for Part Two" $ do
      partTwo "R8,U5,L5,D3\nU7,R6,D4,L4" `shouldBe` 30
    it "works for second sample input for Part Two" $ do
      partTwo
        "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83" `shouldBe`
        610
    it "works for third sample input for Part Two" $ do
      partTwo
        "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7" `shouldBe`
        410
