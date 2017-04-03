module Predicates
  ( checkFinished
  , checkEmpty
  , checkFull
  , checkWin
  , getScore
  , getState
  , validPos
  ) where

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
checkWin c game = or (map isComplete lines)
  where
    lines = getLines game
    isComplete = and . (map (== c))

getState :: Cell -> Grid -> State
getState player game
  | lost = LOST
  | won = WON
  | full = DRAW
  | otherwise = INCOMPLETE
  where
    lost = checkWin (opponent player) game
    won = checkWin player game
    full = checkFull game
    opponent X = O
    opponent O = X

validPos :: Int -> Bool
validPos = between 1 gridSq

getScore :: Depth -> Grid -> Maybe Score
getScore depth game =
  case checkFinished game of
    True -> Just $ score - fudge
    _ -> Nothing
  where
    fudge = depth * signum score
    score = fromEnum . getState computerCell $ game
