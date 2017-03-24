module Predicates ( checkFinished, checkWin, getScore, getState, validPos )  where

-- Imports
import qualified Prelude as P
import           Prelude ((++), (!!), map, and, or, quot)
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

getScore :: Cell -> Grid -> Int -> Int
getScore player game depth = scoreSign * (absScore - depth)
  where
    score = fromEnum $ getState player game
    absScore = abs score
    scoreSign = if score == 0
                  then 0
                  else score `quot` absScore
