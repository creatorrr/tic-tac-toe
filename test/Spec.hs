module Main
  ( main
  ) where

import Prelude
import Test.Hspec

import qualified ConstantsSpec as Constants

main :: IO ()
main = hspec $ do describe "Constants" Constants.spec
