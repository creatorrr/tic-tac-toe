module Predicates
  ( checkFinished
  , checkEmpty
  , checkFull
  , checkWin
  , getScore
  , getState
  , validPos
  ) where

import Data.Boolean

-- Local imports
import Constants
import Types
import Utils

-- Game predicate functions
checkFull :: Grid -> Bool
checkFull = notElem NULL

checkEmpty :: Grid -> Bool
checkEmpty = (== emptyGrid)

checkFinished :: Grid -> Bool
checkFinished = (/= INCOMPLETE) . getState computerCell

checkWin :: Cell -> Grid -> Bool
checkWin c = any (all (== c)) . getLines

getState :: Cell -> Grid -> State
getState player game
  | checkWin player game = WON
  | checkWin (notB player) game = LOST
  | checkFull game = DRAW
  | otherwise = INCOMPLETE

validPos :: Int -> Bool
validPos = between 1 gridSq

getScore :: Depth -> Grid -> Maybe Score
getScore depth game
  | checkFinished game = Just $ score - fudge
  | otherwise = Nothing
  where
    fudge = depth * signum score
    score = fromEnum . getState computerCell $ game
