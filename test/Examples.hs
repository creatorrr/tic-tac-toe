module Examples where

import Control.Arrow ((&&&))
import Test.QuickCheck

import TestUtils

import Constants
import Predicates
import Types
import Utils

side_poss = [2, 4, 6, 8]

corner_poss = [1, 3, 7, 9]

center_pos = 5

genComputerWon = genArbitraryGridBy $ checkWin computer_cell

genPlayerWon = genArbitraryGridBy $ checkWin player_cell

genDrawn =
  genArbitraryGridBy $
  uncurry (&&) . (checkFinished &&& (== DRAW) . getState player_cell)
