module TestUtils
  ( arbitraryGrid
  ) where

import Test.QuickCheck (choose, arbitrary, sized, Gen, Arbitrary)

import Constants
import Types

instance Arbitrary Cell where
  arbitrary = choose (NULL, X)

-- Utils
arbitraryGrid :: Gen [Cell]
arbitraryGrid = sized $ \_ -> sequence [arbitrary | _ <- [1 .. grid_sq]]
