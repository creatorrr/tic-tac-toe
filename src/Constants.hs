module Constants
  ( gridSize
  , gridSq
  , playerCell
  , computerCell
  , emptyGrid
  ) where

-- Local imports
import Types

-- Constants
gridSize :: Int
gridSize = 3

gridSq :: Int
gridSq = gridSize * gridSize

playerCell :: Cell
playerCell = X

computerCell :: Cell
computerCell = O

emptyGrid :: Grid
emptyGrid = [NULL | _ <- [1 .. 9]]
