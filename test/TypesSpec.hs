module TypesSpec where

import Test.Hspec

import Constants
import Types

spec :: Spec
spec =
  describe "State" $ do
    describe "LOST" $ it "should be -100" $ fromEnum LOST `shouldBe` (-100)
    describe "DRAW" $ it "should be 0" $ fromEnum DRAW `shouldBe` 0
    describe "WON" $ it "should be 100" $ fromEnum WON `shouldBe` 100
