module Examples where

import Test.QuickCheck

import TestUtils

import Constants
import Predicates
import Types
import Utils

allPoss = [1 .. gridSq]

sidePoss = [2, 4, 6, 8]

cornerPoss = [1, 3, 7, 9]

centerPoss = [5]

computerWon = [NULL, X, X, O, O, O, NULL, X, NULL]

playerWon = [NULL, O, O, X, X, X, NULL, NULL, NULL, NULL]

drawGame = [X, X, O, O, O, X, X, O, X]

genSideGame = startGameFrom sidePoss

genCornerGame = startGameFrom cornerPoss

genCenterGame = startGameFrom centerPoss

genComputerWon = genArbitraryGridBy $ checkWin computerCell

genPlayerWon = genArbitraryGridBy $ checkWin playerCell

genFinished = genArbitraryGridBy checkFinished

genDrawn = genArbitraryGridBy checkDrawn
  where
    checkDrawn = (== DRAW) . getState playerCell
