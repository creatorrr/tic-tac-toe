module AI ( max', minimax )  where


-- Imports
import           Prelude ((++), (!!), map, and, concat, zip3)
import           Foundation
import           Data.List (repeat, maximumBy, minimumBy)
import           Data.Function (on)
import           Data.Tuple.Select


-- Local imports
import           Constants
import           Game
import           Predicates
import           Types
import           Utils


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

next' :: [Move] -> [Move]
next' = concat . map (\(p, g, s) -> map (wrap p s . play g) $ movesLeft g) where
    wrap p s g = (p, g, s)

max' :: Depth -> [Move] -> Move
max' d moves = if done' moves
  then best . score d $ moves
  else min' (d+1) $ next' moves

min' :: Depth -> [Move] -> Move
min' d moves = if done' moves
  then worst . score d $ moves
  else max' (d+1) $ next' moves

minimax :: Grid -> Pos
minimax game = sel1 . max' 0 $ allMoves where
  allMoves = zip3 (movesLeft game) (repeat game) (repeat Nothing)
