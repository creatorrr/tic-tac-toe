module ConstantsSpec where

import Prelude
import Test.Hspec

import Constants
import Types

spec :: Spec
spec = do
  describe "Cell" $ do
    describe "player_cell" $ do it "should be X" $ do player_cell `shouldBe` X
    describe "computer_cell" $ do
      it "should be O" $ do computer_cell `shouldBe` O
  describe "Grid" $ do
    describe "size" $ do it "should be 9" $ do grid_sq `shouldBe` 9
    describe "emptyGrid" $ do
      it "should be empty" $ do emptyGrid `shouldSatisfy` (and . map (== NULL))
