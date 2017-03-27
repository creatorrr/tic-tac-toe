module Constants
  ( grid_size
  , grid_sq
  , player_cell
  , computer_cell
  , emptyGrid
  ) where

-- Imports
import Prelude

-- Local imports
import Types

-- Constants
grid_size = 3 :: Int

grid_sq = grid_size * grid_size

player_cell = X

computer_cell = O

emptyGrid :: Grid
emptyGrid = [NULL | _ <- [1 .. 9]]
