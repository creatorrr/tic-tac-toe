module Predicates ( checkFinished, checkWin, getState, validPos )  where

-- Imports
import qualified Prelude as P
import           Prelude ((++), (!!), map, and, or)
import           Foundation
import           System.Random


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

getState :: Cell -> Grid -> State
getState player game = if lost then LOST else if won then WON else DRAW
  where
    lost = checkWin (opponent player) game
    won = checkWin player game

    opponent X = O
    opponent O = X
    opponent NULL = NULL

validPos :: Int -> Bool
validPos = between 1 grid_sq
