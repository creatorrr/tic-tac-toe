-- Imports
import System.Random
import Data.List (findIndex, maximumBy, minimumBy)
import Data.Maybe (fromJust)
import Data.Function (on)


-- Types
data State = LOST | DRAW | WON deriving (Eq, Ord, Bounded, Show, Read)
data Cell = NULL | O | X deriving (Eq, Ord, Enum, Bounded, Read)

instance Show Cell where
    show X = "X"
    show O = "O"
    show NULL = "_"

instance Random Cell where
    random g = case randomR (0, 2) g of
                 (r, g') -> (toEnum r, g')

    randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
                 (r, g') -> (toEnum r, g')

instance Enum State where
    fromEnum s = factor * (index - pivot)
      where
        index = indexOf s

        factor = 100
        pivot = max `quot` 2
        max = indexOf biggest
        indexOf s = fromJust (findIndex (==s) ordered)
        biggest = maxBound :: State
        ordered = [LOST, DRAW, WON]

    toEnum i
      | i <= min = LOST
      | i > min && i < max = DRAW
      | otherwise = WON
      where
        min = fromEnum LOST
        max = fromEnum WON

type Grid = [Cell]


-- Constants
grid_size = 3
grid_sq = grid_size * grid_size

player_cell = X
computer_cell = O

emptyGrid :: Grid
emptyGrid = [ NULL | _ <- [1..9]]


-- Utils
printGrid :: Grid -> String
printGrid [] = ""
printGrid (c1:c2:c3:rest) = show c1 ++ "_|_" ++ show c2 ++ "_|_" ++ show c3
                            ++ "\n" ++ printGrid rest

randomGrid :: (RandomGen g) => g -> Grid
randomGrid g = take grid_sq $ randomRs (X, O) g


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
movesLeft = map fst . filter ((==NULL) . snd) . zip [1..]

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
maxMove = maximumBy (compare `on` snd)

minMove :: [Move] -> Move
minMove = minimumBy (compare `on` snd)

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
bestMove = maximumBy (compare `on` snd)

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
minMove' game previous = (previous, snd . bestMove . (map getOutcome) . movesLeft $ game)
  where
    getOutcome pos = maxMove' (play game pos) pos


-- Main
main = putStrLn .getOutcome pos = minMove' (play game pos) pos printGrid $ emptyGrid
