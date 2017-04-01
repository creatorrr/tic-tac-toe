module AI
  ( genMoves
  , next'
  , max'
  , min'
  , minimax
  ) where

import Data.Function (on)
import Data.List (repeat, maximumBy, minimumBy)
import Data.Tuple.Select

-- Local imports
import Constants
import Game
import Predicates
import Types
import Utils

-- AI functions
allFinished :: [Grid] -> Bool
allFinished = all checkFull

allScored :: [Move] -> Bool
allScored = all ((/= Nothing) . sel3)

best :: [Move] -> Move
best = maximumBy (compare `on` sel3)

worst :: [Move] -> Move
worst = minimumBy (compare `on` sel3)

score :: Depth -> [Move] -> [Move]
score d = map (\(p, g, _) -> (p, g, getScore d g))

next' :: [Move] -> [[Move]]
next' = map expand
  where
    expand (p, g, Nothing) = map (wrap p Nothing . play g) $ posLeft g
    expand (p, g, s) = [(p, g, s)]
    wrap p s g' = (p, g', s)

genMoves :: Grid -> [Move]
genMoves game = zip3 nextPoss nextGames (repeat Nothing)
  where
    nextPoss = posLeft game
    nextGames = zipWith play (repeat game) nextPoss

max' :: Depth -> [Move] -> Move
max' d moves =
  best $
  if allScored moves
    then moves
    else map (min' (d + 1) . score d) . next' $ moves

min' :: Depth -> [Move] -> Move
min' d moves =
  worst $
  if allScored moves
    then moves
    else map (max' (d + 1) . score d) . next' $ moves

minimax :: Grid -> Pos
minimax = sel1 . max' 0 . genMoves
