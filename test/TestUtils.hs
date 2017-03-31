module TestUtils
  ( arbitraryGrid
  , gridGen
  ) where

import Test.QuickCheck (choose, arbitrary, sized, Gen, Arbitrary)

import Constants
import Game
import Types

instance Arbitrary Cell where
  arbitrary = choose (NULL, X)

-- Utils
arbitraryGrid :: Gen [Cell]
arbitraryGrid = sized . squash grid_sq $ gridGen
  -- gridGen 0 = return emptyGrid
  -- gridGen n = play n `fmap` gridGen . decc $ n
  where
    squash n f = f . (flip rem $ n)

gridGen _ = sequence [arbitrary | _ <- [1 .. grid_sq]]
