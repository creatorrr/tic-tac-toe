module ConstantsSpec where

import Test.Hspec

import Constants
import Types

spec :: Spec
spec = do
  describe "Cell" $ do
    describe "player_cell" $ do it "should be X" $ do player_cell `shouldBe` X
