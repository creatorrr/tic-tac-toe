module Types ( Pos, Score, Move, State(LOST,DRAW,WON), Cell(NULL,O,X), Grid )  where

-- Imports
import qualified Prelude as P
import           Foundation
import           System.Random
import           Data.List (findIndex)
import           Data.Maybe (fromJust)


-- Types
data State = LOST | DRAW | WON deriving (Eq, Ord, Bounded, P.Show, P.Read)
data Cell = NULL | O | X deriving (Eq, Ord, Enum, Bounded, P.Read)

instance P.Show Cell where
    show X = "X"
    show O = "O"
    show NULL = "_"

instance Random Cell where
    random g = case randomR (0, 2) g of
                 (r, g') -> (toEnum r, g')

    randomR (a, b) g = case randomR (fromEnum a, fromEnum b) g of
                 (r, g') -> (toEnum r, g')

instance Enum State where
    fromEnum s = factor * (index - pivot)
      where
        index = indexOf s

        factor = 100
        pivot = max `P.quot` 2
        max = indexOf biggest
        indexOf s = fromJust (findIndex (==s) ordered)
        biggest = maxBound :: State
        ordered = [LOST, DRAW, WON]

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
type Move = (Pos, Score)
