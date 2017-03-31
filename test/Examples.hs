module Examples where

import Control.Arrow ((&&&))
import Test.QuickCheck

import TestUtils

import Constants
import Predicates
import Types
import Utils

sidePoss = [2, 4, 6, 8]

cornerPoss = [1, 3, 7, 9]

centerPoss = [5]

computerWon = [NULL, X, X, O, O, O, NULL, X, NULL]

playerWon = [NULL, O, O, X, X, X, NULL, NULL, NULL, NULL]

genSideGame = startGameFrom sidePoss

genCornerGame = startGameFrom cornerPoss

genCenterGame = startGameFrom centerPoss

genComputerWon = genArbitraryGridBy $ checkWin computer_cell

genPlayerWon = genArbitraryGridBy $ checkWin player_cell

genDrawn = genArbitraryGridBy checkDrawn
  where
    checkDrawn =
      uncurry (&&) . (checkFinished &&& (== DRAW) . getState player_cell)
