module Game
  ( play
  ) where

import Foundation
import Prelude ((++), (!!))

-- Local imports
import Constants
import Predicates
import Types
import Utils

-- Game functions
play :: Grid -> Pos -> Grid
play game pos =
  if exists
    then error "Invalid move"
    else (take i game) ++ [nextTurn game] ++ (drop pos game)
  where
    i = pos - 1
    existing = game !! i
    exists = existing /= NULL
