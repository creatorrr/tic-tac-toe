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
import System.Random
import Test.QuickCheck

-- Types
data State
  = LOST
  | DRAW
  | WON
  deriving (Eq, Ord, Bounded, Show, Read)

data Cell
  = NULL
  | O
  | X
  deriving (Eq, Ord, Enum, Bounded, Read)

instance Show Cell where
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

instance Arbitrary Cell where
  arbitrary = choose (NULL, X)

-- Aliases
type Pos = Int

type Score = Int

type Depth = Int

type Move = (Pos, Grid, Maybe Score)
