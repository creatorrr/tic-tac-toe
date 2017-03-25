module Game ( play )  where

-- Imports
import qualified Prelude as P
import           Prelude ((++), (!!), map, and, unzip3)
import           Foundation
import           System.Random
import           Data.List (maximumBy, minimumBy)
import           Data.Function (on)
import           Data.Tuple.Select


-- Local imports
import           Constants
import           Predicates
import           Types
import           Utils


-- Game functions
play :: Grid -> Int -> Grid
play game pos = if exists
               then error "Invalid move"
               else (take i game) ++ [nextTurn game] ++ (drop pos game)
  where
    i = pos - 1
    existing = game !! i
    exists = existing /= NULL

-- AI functions
allFinished :: [Grid] -> Bool
allFinished = and . map checkFinished

best :: [Move] -> Move
best = maximumBy (compare `on` sel3)

worst :: [Move] -> Move
worst = minimumBy (compare `on` sel3)

score :: Depth -> [Move] -> [Move]
score d = map (\(p, g, _) -> (p, g, getScore computer_cell d g))

-- max :: Depth -> [Move] -> Move
-- max d moves = if done then best . score d $ moves else min (d+1) nextMoves where
-- 
--   (poss, grids, scores) = unzip3 moves
--   done = allFinished grids
--   nextGrids = 
--   nextMoves = 
