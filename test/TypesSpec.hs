module TypesSpec where

import Prelude
import Test.Hspec

import Constants
import Types

spec :: Spec
spec = do
  describe "State" $ do
    describe "LOST" $ do
      it "should be -100" $ do fromEnum LOST `shouldBe` (-100)
    describe "DRAW" $ do it "should be 0" $ do fromEnum DRAW `shouldBe` 0
    describe "WON" $ do it "should be 100" $ do fromEnum WON `shouldBe` 100
