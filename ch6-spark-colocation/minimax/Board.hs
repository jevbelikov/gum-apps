{-# LANGUAGE BangPatterns #-}
module Board where

import Wins

import Data.List
#if defined(STRATEGIES)
# if defined(EVAL_STRATEGIES)
-- import Parallel -- only used in test setup, not in strategy
import Control.Parallel.Strategies
# elif defined(SM_STRATEGIES)
import Control.Parallel
import Control.DeepSeq
import Control.Parallel.Strategies
# else 
import Control.Parallel
import Control.Parallel.Strategies
# endif
#else
import GHC.Conc -- hiding (pseq,par)
#endif

--boardDim = 4

type Board = [Row] 
type Row = [Piece]
data Piece = X | O | Empty deriving (Eq,Show)

isEmpty Empty = True
isEmpty _     = False

showBoard :: Board -> String
showBoard board = intercalate "\n--------\n" (map showRow board) ++ "\n"
 where showRow r = intercalate "|" (map showPiece r)

showPiece :: Piece -> String
showPiece X = "X"
showPiece O = "O"
showPiece Empty = " "

placePiece :: Piece -> Board -> (Int,Int) -> Board
placePiece new board pos
  = [[ if (x,y) == pos then new else old
     | (x,old) <- zip [1..] row ]
     | (y,row) <- zip [1..] board ]

empty :: (Int,Int) -> Board -> Bool
empty (x,y) board = isEmpty ((board !! (y-1)) !! (x-1))

fullBoard b = all (not.isEmpty) (concat b)

newPositions :: Piece -> Board -> [Board]
newPositions piece board = 
--  [ placePiece piece board (x,y) | (x,y) <- empties board ]
    goRows piece id board

goRows p rowsL [] = []
goRows p rowsL (row:rowsR) 
  = goRow p rowsL id row rowsR ++ goRows p (rowsL . (row:)) rowsR

goRow p rowsL psL [] rowsR = []
goRow p rowsL psL (Empty:psR) rowsR
    = (rowsL $ (psL $ (p:psR)) : rowsR) : goRow p rowsL (psL . (Empty:)) psR rowsR
goRow p rowsL psL (p':psR) rowsR = goRow p rowsL (psL . (p':)) psR rowsR

empties board = [ (x,y) | (y,row)   <- zip [1..] board,
                          (x,Empty) <- zip [1..] row ]


initialBoard :: Int -> Board
initialBoard boardDim = replicate boardDim (replicate boardDim Empty)

data Evaluation = OWin | Score {-# UNPACK #-}!Int | XWin
  -- higher scores denote a board in X's favour
  deriving (Show,Eq)

maxE :: Evaluation -> Evaluation -> Evaluation
maxE XWin _ = XWin
maxE _ XWin = XWin
maxE b OWin = b
maxE OWin b = b
maxE a@(Score x) b@(Score y) 	| x>y = a
			 	| otherwise = b

minE :: Evaluation -> Evaluation -> Evaluation
minE OWin _ = OWin
minE _ OWin = OWin
minE b XWin = b
minE XWin b = b
minE a@(Score x) b@(Score y) 	| x<y = a
				| otherwise = b

eval n boardDim | n  == boardDim = XWin                              
                | -n == boardDim = OWin          
                | otherwise      = Score n

static :: Int -> Board -> Evaluation
static boardDim board  = interpret 0 (score board boardDim)

interpret :: Int -> [Evaluation] -> Evaluation
interpret x [] = (Score x)
interpret x (Score y:l) = interpret (x+y) l
interpret x (XWin:l) = XWin
interpret x (OWin:l) = OWin

scorePiece X     = 1
scorePiece O     = -1
scorePiece Empty = 0

scoreString !n [] = n
scoreString !n (X:ps)     = scoreString (n+1) ps
scoreString !n (O:ps)     = scoreString (n-1) ps
scoreString !n (Empty:ps) = scoreString n ps

score :: Board -> Int -> [Evaluation]
score board boardDim = 
   [ eval (scoreString 0 row) boardDim | row <- board ] ++
   [ eval (scoreString 0 col) boardDim | col <- transpose board ] ++
   [ eval (scoreString 0 (zipWith (!!) board [0..])) boardDim ,
     eval (scoreString 0 (zipWith (!!) board [boardDim-1,boardDim-2 ..])) boardDim ]

