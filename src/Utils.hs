module Utils
  ( between
  , pick
  , intersperse
  , decc
  , printGrid
  , getLines
  , getRows
  , getCols
  , getDiags
  , posLeft
  , nextTurn
  ) where

import Control.Monad

import Constants
import Types

-- Utils
pick :: [Int] -> [a] -> [a]
pick [] _ = []
pick (x:xs) l = (l !! x) : pick xs l

intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse c (a:rest) = a : c : intersperse c rest

printGrid :: Grid -> String
printGrid [] = ""
printGrid (c1:c2:c3:rest) =
  (++ "\n" ++ printGrid rest) . join . intersperse "_|_" . map show $
  [c1, c2, c3]

between :: Int -> Int -> (Int -> Bool)
between low high num = num >= low && num <= high

decc :: Int -> Int
decc n = n - 1

posLeft :: Grid -> [Int]
posLeft = map fst . filter ((== NULL) . snd) . zip [1 ..]

nextTurn :: Grid -> Cell
nextTurn game
  | odd $ length . posLeft $ game = X
  | otherwise = O

iMax :: Pos
iMax = decc gridSize

allIs :: [Pos]
allIs = [0 .. iMax]

getCols :: Grid -> [[Cell]]
getCols g = map ($ g) $ getCol <$> allIs
  where
    getCol i = pick $ (+ i) . (* gridSize) <$> allIs

getRows :: Grid -> [[Cell]]
getRows g = map ($ g) $ getRow <$> allIs
  where
    getRow i = pick $ (+ (i * gridSize)) <$> allIs

getDiags :: Grid -> [[Cell]]
getDiags g = [getPrimaryDiag g, getSecondaryDiag g]
  where
    getPrimaryDiag = pick $ (* succ gridSize) <$> allIs
    getSecondaryDiag = pick [pos * gridSize + iMax - pos | pos <- allIs]

getLines :: Grid -> [[Cell]]
getLines g = join $ ($ g) <$> [getCols, getRows, getDiags]
