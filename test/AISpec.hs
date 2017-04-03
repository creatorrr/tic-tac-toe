module AISpec where

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
spec =
  describe "AI" $
  describe "minimax" $
  it "should never lose" $ do
    let results = playSelf . play emptyGrid <$> [1 .. gridSq]
    getState computerCell <$> results `shouldSatisfy` notElem LOST
