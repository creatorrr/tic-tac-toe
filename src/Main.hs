module Main where


-- Imports
import qualified Prelude as P
import           Prelude ((!!), (++), map)
import           Foundation
import           System.Random
import           Data.List (maximumBy, minimumBy)
import           Data.Function (on)


-- Local imports
import           Constants
import           Game
import           Predicates
import           Types
import           Utils


-- AI functions
maxMove :: [Move] -> Move
maxMove = maximumBy (compare `on` P.snd)

minMove :: [Move] -> Move
minMove = minimumBy (compare `on` P.snd)

rankedMoves :: Grid -> [Move]
rankedMoves game = map getOutcome $ movesLeft game
  where
    getOutcome pos = minimax $ (nextGame pos, pos)
    nextGame = play game

minimax :: (Grid, Pos) -> Move
minimax (game, selectedPos)
  | checkFinished game = (selectedPos, getScore computer_cell game 0)
  | otherwise = best $ rankedMoves game

    where
      best = if nextTurn game == computer_cell then maxMove else minMove


-- Attempt #2
bestMove = maximumBy (compare `on` P.snd)

minimax' :: Grid -> Maybe Pos
minimax' game = if validPos selectedPos
                  then Just selectedPos
                  else Nothing
  where
    (selectedPos, score) = maxMove' game (-1)

maxMove' :: Grid -> Pos -> Move
maxMove' game previous
  | checkFinished game = (previous, getScore computer_cell game 0)
  | otherwise = bestMove . (map getOutcome) . movesLeft $ game
    where
      getOutcome pos = minMove' (play game pos) pos

minMove' :: Grid -> Pos -> Move
minMove' game previous = (previous, P.snd . bestMove . (map getOutcome) . movesLeft $ game)
  where
    getOutcome pos = maxMove' (play game pos) pos


-- Main
main :: IO ()
main = putStrLn "Unimplemented"
