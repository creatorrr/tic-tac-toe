module TestUtils
  ( arbitraryGrid
  , genArbitraryGridBy
  ) where

import Test.QuickCheck
       (choose, generate, arbitrary, sized, Gen, Arbitrary)

import Constants
import Game
import Types

instance Arbitrary Cell where
  arbitrary = choose (NULL, X)

-- Utils
arbitraryGrid :: Gen [Cell]
arbitraryGrid = sized . squash grid_sq $ gridGen
  where
    gridGen 0 = return emptyGrid
    gridGen n = (flip play $ n) <$> gridGen (n - 1)
    squash n f = f . (flip rem $ n)

genArbitraryGridBy pred = do
  g <- generate arbitraryGrid
  if pred g
    then return g
    else genArbitraryGridBy pred
