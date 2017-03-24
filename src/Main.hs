module Main where

-- Imports
import qualified Prelude as P
import           Prelude ((!!), (++), map, quot, and, or, zip, rem)
import           Foundation
import           System.Random
import           Data.List (maximumBy, minimumBy)
import           Data.Function (on)


-- Local imports
import           Constants
import           Types
import           Utils


-- Game predicate functions
getLines :: Grid -> [[Cell]]
getLines g = getCols g ++ getRows g ++ getDiags g
  where
    i_max = grid_size - 1

    getCol i g = [ g !! (pos + i) | pos <- map (*grid_size) [0..i_max] ]
    getRow i g = [ g !! (i*grid_size + pos) | pos <- [0..i_max] ]

    getCols g = [ getCol pos g | pos <- [0..i_max] ]
    getRows g = [ getRow pos g | pos <- [0..i_max] ]

    getPrimaryDiag g = [ g !! ((*pos) . succ $ grid_size) | pos <- [0..i_max] ]
    getSecondaryDiag g = [ g !! (pos * grid_size + i_max - pos) | pos <- [0..i_max] ]

    getDiags g = getPrimaryDiag g : getSecondaryDiag g : []

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

movesLeft :: Grid -> [Int]
movesLeft = map fst . filter ((==NULL) . P.snd) . zip [1..]

nextTurn :: Grid -> Cell
nextTurn game
  | (length . movesLeft $ game) `rem` 2 /= 0 = X
  | otherwise = O

between :: Int -> Int -> (Int -> Bool)
between low high num = num >= low && num <= high

validPos :: Int -> Bool
validPos = between 1 grid_sq


-- Game functions
play :: Grid -> Int -> Grid
play game pos = if exists
               then error "Invalid move"
               else (take i game) ++ [nextTurn game] ++ (drop pos game)
  where
    i = pos - 1
    existing = game !! i
    exists = existing /= NULL


-- AI functions
type Pos = Int
type Score = Int
type Move = (Pos, Score)

getScore :: Cell -> Grid -> Int -> Int
getScore player game depth = scoreSign * (absScore - depth)
  where
    score = fromEnum $ getState player game
    absScore = abs score
    scoreSign = if score == 0
                  then 0
                  else score `quot` absScore

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
