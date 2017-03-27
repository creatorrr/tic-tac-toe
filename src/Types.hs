module Types
  ( Depth
  , Pos
  , Score
  , Move
  , State(LOST, DRAW, WON)
  , Cell(NULL, O, X)
  , Grid
  ) where

import Data.List (findIndex)
import Data.Maybe (fromJust)
import Foundation
-- Imports
import qualified Prelude as P
import System.Random

-- Types
data State
  = LOST
  | DRAW
  | WON
  deriving (Eq, Ord, Bounded, P.Show, P.Read)

data Cell
  = NULL
  | O
  | X
  deriving (Eq, Ord, Enum, Bounded, P.Read)

instance P.Show Cell where
  show X = "X"
  show O = "O"
  show NULL = "_"

instance Random Cell where
  random g =
    case randomR (0, 2) g of
      (r, g') -> (toEnum r, g')
  randomR (a, b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (r, g') -> (toEnum r, g')

instance Enum State where
  fromEnum s =
    factor *
    case s of
      LOST -> -1
      DRAW -> 0
      WON -> 1
    where
      factor = 100
  toEnum i
    | i <= min = LOST
    | i > min && i < max = DRAW
    | otherwise = WON
    where
      min = fromEnum LOST
      max = fromEnum WON

type Grid = [Cell]

-- Aliases
type Pos = Int

type Score = Int

type Depth = Int

type Move = (Pos, Grid, Maybe Score)
