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
import Foundation
import Prelude ((++), (!!), map, and, zip3, zipWith, concat)

-- Local imports
import Constants
import Game
import Predicates
import Types
import Utils

-- AI functions
allFinished :: [Grid] -> Bool
allFinished = and . map checkFinished

best :: [Move] -> Move
best = maximumBy (compare `on` sel3)

worst :: [Move] -> Move
worst = minimumBy (compare `on` sel3)

score :: Depth -> [Move] -> [Move]
score d = map (\(p, g, _) -> (p, g, getScore d g))

done' :: [Move] -> Bool
done' = allFinished . map sel2

next' :: [Move] -> [[Move]]
next' = map (\(p, g, s) -> map (wrap p s . play g) $ posLeft g)
  where
    wrap p s g' = (p, g', s)

genMoves :: Grid -> [Move]
genMoves game = zip3 nextPoss nextGames (repeat Nothing)
  where
    nextPoss = posLeft game
    nextGames = zipWith play (repeat game) nextPoss

max' :: Depth -> [Move] -> Move
max' d moves =
  best . score d $
  if done' moves
    then moves
    else map (min' (d + 1)) $ next' moves

min' :: Depth -> [Move] -> Move
min' d moves =
  worst . score d $
  if done' moves
    then moves
    else map (max' (d + 1)) $ next' moves

minimax :: Grid -> Pos
minimax = sel1 . max' 0 . genMoves
