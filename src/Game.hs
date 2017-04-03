module Game
  ( play
  ) where

-- Local imports
import Types
import Utils

-- Game functions
play :: Grid -> Pos -> Grid
play game pos
  | exists pos = error "Invalid move"
  | otherwise = take (decc pos) game ++ [nextTurn game] ++ drop pos game
  where
    exists = (/= NULL) . (!!) game . decc
