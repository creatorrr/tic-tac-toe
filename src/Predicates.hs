module Predicates ( checkFinished, checkWin, getScore, validPos )  where

-- Imports
import qualified Prelude as P
import           Prelude (map, and, or)
import           Foundation


-- Local imports
import           Constants
import           Types
import           Utils

-- Game predicate functions
checkFinished :: Grid -> Bool
checkFinished = and . map (/= NULL)

checkWin :: Cell -> Grid -> Bool
checkWin c game = or (map isComplete lines)
  where
    lines = getLines game
    isComplete = and . (map (==c))

validPos :: Int -> Bool
validPos = between 1 grid_sq

getScore :: Depth -> Grid -> Maybe Score
getScore depth game = case checkFinished game of
  True -> Just $ score - depth
  _ -> Nothing
  
  where
    score = fromEnum $ checkWin computer_cell game
