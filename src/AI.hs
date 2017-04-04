module AI
  ( play
  , minimax
  ) where

import Control.Lens (view, set, _1, _2, _3)
import Data.Function (on)
import Data.List (repeat, maximumBy, minimumBy)

-- Local imports
import Predicates
import Types
import Utils

-- Game play function
play :: Grid -> Pos -> Grid
play game pos
  | exists pos = error "Invalid move"
  | otherwise = take (decc pos) game ++ [nextTurn game] ++ drop pos game
  where
    exists = (/= NULL) . (!!) game . decc

-- AI functions
allFinished :: [Move] -> Bool
allFinished = all (checkFinished . view _2)

best :: [Move] -> Move
best = maximumBy (compare `on` view _3)

worst :: [Move] -> Move
worst = minimumBy (compare `on` view _3)

score :: Depth -> [Move] -> [Move]
score d = map $ flip (set _3) <*> getScore d . view _2

next' :: [Move] -> [[Move]]
next' = map expand
  where
    expand (p, g, Nothing) = (wrap p Nothing . play g) <$> posLeft g
    expand (p, g, s) = [(p, g, s)]
    wrap p s g' = (p, g', s)

genMoves :: Grid -> [Move]
genMoves game = zip3 nextPoss nextGames $ repeat Nothing
  where
    nextPoss = posLeft game
    nextGames = play game <$> nextPoss

max' :: Depth -> [Move] -> Move
max' d moves
  | allFinished moves = best moves
  | otherwise = best $ min' (d + 1) . score d <$> next' moves

min' :: Depth -> [Move] -> Move
min' d moves
  | allFinished moves = worst moves
  | otherwise = worst $ max' (d + 1) . score d <$> next' moves

minimax :: Grid -> Pos
minimax = view _1 . max' 0 . genMoves
