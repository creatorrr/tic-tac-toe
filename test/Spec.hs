module Main
  ( main
  ) where

import Prelude
import Test.Hspec

import qualified ConstantsSpec as Constants
import qualified TypesSpec as Types

main :: IO ()
main =
  hspec $ do
    describe "Constants" Constants.spec
    describe "Types" Types.spec
