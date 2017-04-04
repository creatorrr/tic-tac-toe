module Game
  ( startGame
  , makePlayerMove
  , makeAIMove
  , printGame
  ) where

import Control.Conditional (if')
import Control.Monad (liftM2)

-- Local imports
import AI
import Constants
import Predicates
import Types
import Utils

-- Game functions
startGame :: Grid -> IO ()
startGame grid =
  printPrompt <$> printGame grid >>= makePlayerMove >>= makeAIMove

printGame :: Grid -> IO Grid
printGame = putStrLn_ <*> showGrid

printPrompt :: Grid -> IO Grid
printPrompt = putStrLn_ <*> ((++) "Select position from " . show . posLeft)

makePlayerMove :: IO Grid -> IO Grid
makePlayerMove = liftM2 play <*> pure (read <$> getLine)

makeAIMove :: Grid -> IO ()
makeAIMove =
  if' <$> checkFinished <*> putStrLn . (++) "You " . show . getState playerCell <*>
  startGame . (play <*> minimax)
