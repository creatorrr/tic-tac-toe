module Utils ( printGrid, randomGrid )  where

-- Imports
import qualified Prelude as P
import           Prelude ((++))
import           Foundation
import           System.Random


-- Local imports
import           Types
import           Constants


-- Utils
printGrid :: Grid -> [Char]
printGrid [] = ""
printGrid (c1:c2:c3:rest) = P.show c1 ++ "_|_" ++ P.show c2 ++ "_|_" ++ P.show c3
                            ++ "\n" ++ printGrid rest

randomGrid :: (RandomGen g) => g -> Grid
randomGrid g = take grid_sq $ randomRs (X, O) g
