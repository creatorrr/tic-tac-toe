module Examples where

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

drawGame = [X, X, O, O, O, X, X, O, X]

genSideGame = startGameFrom sidePoss

genCornerGame = startGameFrom cornerPoss

genCenterGame = startGameFrom centerPoss

genComputerWon = genArbitraryGridBy $ checkWin computer_cell

genPlayerWon = genArbitraryGridBy $ checkWin player_cell

genFinished = genArbitraryGridBy checkFinished

genDrawn = genArbitraryGridBy checkDrawn
  where
    checkDrawn = (== DRAW) . getState player_cell
