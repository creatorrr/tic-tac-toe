module TestUtils
  ( arbitraryGrid
  , genArbitraryGridBy
  , startGameFrom
  ) where

import Test.QuickCheck
       (choose, generate, arbitrary, sized, Gen, Arbitrary)

import Constants
import Game
import Types
import Utils

instance Arbitrary Cell where
  arbitrary = choose (NULL, X)

-- Utils
arbitraryGrid :: Gen [Cell]
arbitraryGrid = sized . squash grid_sq $ gridGen
  where
    gridGen 0 = return emptyGrid
    gridGen n = (flip play $ n) <$> gridGen (n - 1)
    squash n f = f . (flip rem $ n)

genArbitraryGridBy :: (Grid -> Bool) -> IO Grid
genArbitraryGridBy pred = do
  g <- generate arbitraryGrid
  if pred g
    then return g
    else genArbitraryGridBy pred

startGameFrom :: [Pos] -> IO Grid
startGameFrom ls = play emptyGrid <$> pos
  where
    pos = generate $ flip (!!) <$> choose (0, decc . length $ ls) <*> pure ls
