module Utils
  ( arbitraryGrid
  , between
  , decc
  , printGrid
  , randomGrid
  , getLines
  , getRows
  , getCols
  , getDiags
  , posLeft
  , nextTurn
  , previousTurn
  ) where

import System.Random
import Test.QuickCheck (arbitrary, sized, Gen, Arbitrary)

import Constants
import Types

-- Utils
printGrid :: Grid -> [Char]
printGrid [] = ""
printGrid (c1:c2:c3:rest) =
  show c1 ++ "_|_" ++ show c2 ++ "_|_" ++ show c3 ++ "\n" ++ printGrid rest

randomGrid
  :: (RandomGen g)
  => g -> Grid
randomGrid g = take grid_sq $ randomRs (NULL, X) g

arbitraryGrid :: Gen [Cell]
arbitraryGrid = sized $ \_ -> sequence [arbitrary | _ <- [1 .. grid_sq]]

between :: Int -> Int -> (Int -> Bool)
between low high num = num >= low && num <= high

decc :: Int -> Int
decc n = n - 1

posLeft :: Grid -> [Int]
posLeft = map fst . filter ((== NULL) . snd) . zip [1 ..]

nextTurn :: Grid -> Cell
nextTurn game
  | (length . posLeft $ game) `rem` 2 /= 0 = X
  | otherwise = O

previousTurn :: Grid -> Grid -> Maybe Pos
previousTurn game newGame =
  if n_diff == 1
    then Just move
    else Nothing
  where
    diff@(move:_) = map fst' . filter (uncurry (/=) . toPair) $ zipped
    zipped = zip3 [1 ..] game newGame
    n_diff = length diff
    toPair = \(a, b, c) -> (b, c)
    fst' = \(a, _, _) -> a

i_max :: Pos
i_max = grid_size - 1

getCols :: Grid -> [[Cell]]
getCols g = [getCol pos g | pos <- [0 .. i_max]]
  where
    getCol i g = [g !! (pos + i) | pos <- map (* grid_size) [0 .. i_max]]

getRows :: Grid -> [[Cell]]
getRows g = [getRow pos g | pos <- [0 .. i_max]]
  where
    getRow i g = [g !! (i * grid_size + pos) | pos <- [0 .. i_max]]

getDiags :: Grid -> [[Cell]]
getDiags g = getPrimaryDiag g : getSecondaryDiag g : []
  where
    getPrimaryDiag g = [g !! ((* pos) . succ $ grid_size) | pos <- [0 .. i_max]]
    getSecondaryDiag g =
      [g !! (pos * grid_size + i_max - pos) | pos <- [0 .. i_max]]

getLines :: Grid -> [[Cell]]
getLines g = getCols g ++ getRows g ++ getDiags g
