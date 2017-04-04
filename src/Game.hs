module Game
  ( startGame
  ) where

-- Local imports
import AI
import Constants
import Predicates
import Types
import Utils

-- Game functions
startGame :: Grid -> IO ()
startGame grid = do
  putStrLn . printGrid $ grid
  putStrLn $ "Select position from " ++ show (posLeft grid)
  opt <- read <$> getLine
  let newGrid = play grid opt
  if checkFinished newGrid
    then putStrLn . (++) "You " . show . getState playerCell $ newGrid
    else startGame $ play <*> minimax $ newGrid
