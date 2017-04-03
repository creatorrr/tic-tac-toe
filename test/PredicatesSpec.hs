module PredicatesSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Examples as Eg
import TestUtils

import Constants
import Predicates
import Types
import Utils

spec :: Spec
spec =
  describe "Predicates" $ do
    describe "checkWin" $ do
      it "should check if game won" $ do
        checkWin X Eg.playerWon `shouldBe` True
        checkWin O Eg.computerWon `shouldBe` True
      it "should return false if game drawn" $ do
        player <- generate $ choose (O, X)
        checkWin player Eg.drawGame `shouldBe` False
    describe "getScore" $
      it "should return correct score" $ do
        depth <- generate $ choose (0, 20)
        getScore depth Eg.playerWon `shouldBe` Just (depth - 100)
        getScore depth Eg.computerWon `shouldBe` Just (100 - depth)
        getScore depth Eg.drawGame `shouldBe` Just 0
