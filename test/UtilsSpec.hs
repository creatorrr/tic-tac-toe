module UtilsSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import TestUtils

import Constants
import Types
import Utils

spec :: Spec
spec = do
  describe "Utils" $ do
    describe "posLeft" $ modifyMaxSize (const gridSq) $ do
      it "should return non empty for grids with holes" $ property $ \x ->
        elem NULL x ==> not . null $ posLeft x
      it "should return empty for grids without holes" $ property $ \x ->
        (not . elem NULL $ x) ==> null $ posLeft x
      it "should return correct pos of holes" $ property $ \x ->
        elem NULL x ==> forAll (elements . posLeft $ x) $ (== NULL) . ((!!) x) .
        decc
    describe "getLines" $ do
      it "should return 8 lines" $ do
        grid <- generate arbitraryGrid
        (length . getLines $ grid) `shouldBe` (3 + 3 + 2)
    describe "getCols" $ do
      it "should return 3 lines" $ do
        grid <- generate arbitraryGrid
        (length . getCols $ grid) `shouldBe` 3
      it "should return cols" $ do
        grid <- generate arbitraryGrid
        (getCols grid) `shouldMatchList`
          map ($ grid) [pick [0, 3, 6], pick [1, 4, 7], pick [2, 5, 8]]
    describe "getRows" $ do
      it "should return 3 lines" $ do
        grid <- generate arbitraryGrid
        (length . getRows $ grid) `shouldBe` 3
      it "should return rows" $ do
        grid <- generate arbitraryGrid
        (getRows grid) `shouldMatchList`
          map ($ grid) [pick [0, 1, 2], pick [3, 4, 5], pick [6, 7, 8]]
    describe "getDiags" $ do
      it "should return 2 lines" $ do
        grid <- generate arbitraryGrid
        (length . getDiags $ grid) `shouldBe` 2
      it "should return diags" $ do
        grid <- generate arbitraryGrid
        (getDiags grid) `shouldMatchList`
          map ($ grid) [pick [0, 4, 8], pick [2, 4, 6]]
