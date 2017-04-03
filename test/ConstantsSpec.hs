module ConstantsSpec where

import Test.Hspec

import Constants
import Types

spec :: Spec
spec = do
  describe "Cell" $ do
    describe "playerCell" $ do it "should be X" $ do playerCell `shouldBe` X
    describe "computerCell" $ do
      it "should be O" $ do computerCell `shouldBe` O
  describe "Grid" $ do
    describe "size" $ do it "should be 9" $ do gridSq `shouldBe` 9
    describe "emptyGrid" $ do
      it "should be empty" $ do emptyGrid `shouldSatisfy` (and . map (== NULL))
