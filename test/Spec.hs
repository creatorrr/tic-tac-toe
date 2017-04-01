module Main
  ( main
  ) where

import Prelude
import Test.Hspec

import Examples

import qualified AISpec as AI
import qualified ConstantsSpec as Constants
import qualified PredicatesSpec as Predicates
import qualified TypesSpec as Types
import qualified UtilsSpec as Utils

main :: IO ()
main =
  hspec $ do
    describe "AI" AI.spec
    describe "Constants" Constants.spec
    describe "Predicates" Predicates.spec
    describe "Types" Types.spec
    describe "Utils" Utils.spec
