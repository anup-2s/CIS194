module HW1Spec
  ( spec
  ) where

import HW1
import Test.Hspec

spec :: Spec
spec = do
  exercise1
  exercise2
  exercise3
  -- exercise4
  -- exercise5

exercise1 :: Spec
exercise1 =
  describe "toDigits" $ do
    it "Correctly handles digits" $ toDigits 1234 `shouldBe` [1, 2, 3, 4]
    it "Correctly reverse digits" $ toDigitsRev 1234 `shouldBe` [4, 3, 2, 1]
    it "Handles 0" $ toDigits 0 `shouldBe` []
    it "Handles negatives" $ toDigits (-17) `shouldBe` []

exercise2 :: Spec
exercise2 =
  describe "doubleEveryOther" $ do
    it "doubles an even length list" $ doubleEveryOther [8, 7, 6, 5] `shouldBe` [16, 7, 12, 5]
    it "doubles an odd length list" $ doubleEveryOther [1, 2, 3] `shouldBe` [1, 4, 3]

exercise3 :: Spec
exercise3 =
  describe "sumDigits" $ it "sums with tens" $ sumDigits [16,7,12,5] `shouldBe` 22
