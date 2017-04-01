module AISpec where

import System.Random
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Examples as Eg
import TestUtils

import AI
import Constants
import Game
import Predicates
import Types
import Utils

spec :: Spec
spec = do
  describe "AI" $ do
    describe "minimax" $
      it "should win each time" $ do
        begin <- generate $ choose (1, grid_sq)
        let result = playSelf $ play emptyGrid begin
        getState computer_cell result `shouldBe` WON
