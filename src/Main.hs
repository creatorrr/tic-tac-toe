module Main where

-- Local imports
import AI
import Constants
import Game
import Predicates
import Types
import Utils

-- Main
main :: IO ()
main = putStrLn . show . minimax $ play emptyGrid 1
