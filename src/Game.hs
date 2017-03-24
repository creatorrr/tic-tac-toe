module Game ( play )  where

-- Imports
import           Prelude ((++), (!!))
import           Foundation
import           System.Random


-- Local imports
import           Constants
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
