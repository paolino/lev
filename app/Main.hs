module Main (main) where

import Sudoku ( run )
import Sudoku.Boards
    ( medium, hard, expert, evil, gold17, test, simple )

main :: IO () 
main = do 
    run simple
    run medium
    run hard
    run expert
    run evil 
    run gold17
    run test
