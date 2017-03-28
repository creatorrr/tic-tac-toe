module UtilsSpec where

import Prelude
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Constants
import Types
import Utils

spec :: Spec
spec = do
  describe "Utils" $ do
    describe "posLeft" $ modifyMaxSize (const 9) $ do
      it "should return non empty for grids with holes" $ property $ \x ->
        elem NULL x ==> not . null $ posLeft x
      it "should return empty for grids without holes" $ property $ \x ->
        (not . elem NULL $ x) ==> null $ posLeft x
