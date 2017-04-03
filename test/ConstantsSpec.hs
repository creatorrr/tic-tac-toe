module ConstantsSpec where

import Test.Hspec

import Constants
import Types

spec :: Spec
spec = do
  describe "Cell" $ do
    describe "playerCell" $ it "should be X" $ playerCell `shouldBe` X
    describe "computerCell" $ it "should be O" $ computerCell `shouldBe` O
  describe "Grid" $ do
    describe "size" $ it "should be 9" $ gridSq `shouldBe` 9
    describe "emptyGrid" $
      it "should be empty" $ emptyGrid `shouldSatisfy` all (== NULL)
