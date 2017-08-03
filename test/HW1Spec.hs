module HW1Spec
  ( spec
  ) where

import HW1
import Test.Hspec

spec :: Spec
spec = describe "toDigits" $ do
  it "Correctly handles digits" $ toDigits 1234 `shouldBe` [1, 2, 3, 4]
  it "Correctly reverse digits" $ toDigitsRev 1234 `shouldBe` [4,3,2,1]
  it "Handles 0" $ toDigits 0 `shouldBe` []
  it "Handles negatives" $ toDigits (-17) `shouldBe` []
