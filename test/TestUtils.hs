module TestUtils
  ( arbitraryGrid
  , genArbitraryGridBy
  , startGameFrom
  , playSelf
  ) where

import Test.QuickCheck
       (choose, generate, arbitrary, sized, Gen, Arbitrary)

import AI
import Constants
import Game
import Predicates
import Types
import Utils

instance Arbitrary Cell where
  arbitrary = choose (NULL, X)

-- Utils
arbitraryGrid :: Gen [Cell]
arbitraryGrid = sized . squash gridSq $ gridGen
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

playSelf :: Grid -> Grid
playSelf grid
  | checkEmpty grid = error "Empty grid"
  | otherwise =
    if checkFinished grid
      then grid
      else playSelf $ play <*> minimax $ grid
