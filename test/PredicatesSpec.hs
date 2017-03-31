module PredicatesSpec where

import System.Random
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
spec = do
  describe "Predicates" $ do
    describe "checkFinished" $
      it "should check if game finished" $ do
        result <- checkFinished . randomGrid <$> newStdGen
        result `shouldBe` True
    describe "checkWin" $ do
      it "should check if game won" $ do
        let playerResult = checkWin X Eg.playerWon
        let computerResult = checkWin O Eg.computerWon
        playerResult && computerResult `shouldBe` True
      it "should return false if game drawn" $ do
        player <- generate $ choose (O, X)
        let drawResult = checkWin player Eg.drawGame
        drawResult `shouldBe` False
    describe "getScore" $
      it "should return correct score" $ do
        depth <- generate $ choose (0, 20)
        let playerResult = getScore depth Eg.playerWon
        let computerResult = getScore depth Eg.computerWon
        let drawResult = getScore depth Eg.drawGame
        computerResult `shouldBe` Just (100 - depth)
        playerResult `shouldBe` Just (depth - 100)
        drawResult `shouldBe` Just 0
