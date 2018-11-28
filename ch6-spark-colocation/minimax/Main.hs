-- Time-stamp: <2009-05-06 13:54:34 simonmar>
-----------------------------------------------------------------------------

module Main where

import System.Environment
import Prog
import Board
import System.Random

-- arguments:
-- n board size (square boeard)
-- m moves preplaced
-- depth of search
-- t inner threshold
main = do
  [n, m, depth, t] <- fmap (map read) getArgs
  setStdGen (mkStdGen 99999)
  b <- randomBoard n m
  putStrLn $ showBoard b
  putStrLn $ solve depth t b n
