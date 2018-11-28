-- Time-stamp: <Wed Sep 29 2010 20:55:16 Stardate: Stardate: [-28]3758.94 hwloidl>
-----------------------------------------------------------------------------

module Game where

import Board
import Tree


import GHC.Base --(par#, Int#, parEnc#,I#)
import GHC.Conc --(par, pseq, I#)     


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

type Player = Evaluation -> Evaluation -> Evaluation
type Move = (Board,Evaluation)

--
--  old strategies -- less compositional
--

type Strat a = a -> ()

x `usingOld` s = s x `GHC.Conc.pseq` x

rwhnf x = x `GHC.Conc.pseq` ()

parListOld s [] = ()
parListOld s (x:xs) = s x `GHC.Conc.par` (parListOld s xs) 


parListEnc :: Int -> Strat a -> Strat [a]
parListEnc e s [] = ()
parListEnc e s (x:xs) = (myparEnc n m (fromI# e))
  where n = s x 
        m = parListEnc (e-1) s xs

-------------------------------
-- BEGIN parEnc stuff
-------------------------------

-- import statement must come before type declarations (as above)
--import GHC.Base  --(par#, parGlobal#, Int#, parEnc#)
--import GHC.Conc --(par, pseq, I#)                   

-- helper function: create and unboxed Int from Int
{-# INLINE fromI# #-}
fromI# :: (Integral n) => n -> Int#
fromI# n = n#  where !(I# n#) = fromIntegral n

mypar :: a -> b -> b
mypar x y = case (par# x) of { i' -> y }

-- a -> Int# -> Int# -> Int# -> Int# -> b -> Int#
--myparGlobal :: Num b => a -> b -> b
--myparGlobal x y = case (parGlobal# x 0# 0# 0# 0# y) of { i' -> y }
{-
[ 5 of 10] Compiling Tree             ( Tree.hs, Tree.pp_P2_O2_o )
ghc-stage1: panic! (the 'impossible' happened)
  (GHC version 6.12.3.20140825 for x86_64-unknown-linux):
	emitPrimOp: can't translate PrimOp parGlobal#{v}
-}

--myparEnc :: a -> b -> Int# -> b
-- annotation version
myparEnc x y e = case (parEnc# x e 2# y) of {i' -> if (I# i')==0 then y else y}  -- base 2  was : then 1 else y

-- startegy version
-- tag as first arg to allow for partial evaluation
myrparEnc e x = myparEnc x (return x) (fromI# e)
   
 
 
-- inner strat
--myParListEnc :: [(Int,a)] -> Strategy a -> Strategy a                             -- outer strat: zip with Int for a more compositional version inner: call parEnc 
--myParListEnc [] = ()
--myParList ((index,x):xs) = myparEnc x (myParListEnc xs) index

-- linear recusion: less compositional but avoids intermediate list generation
-- matching parList strategy type (old start style)
--myParListEncLinRec :: Int -> Strategy a -> Strategy [a]     
--myParListEncLinRec  _  strat []     = ()
--myParListEncLinRec tag strat (x:xs) = myparEnc (strat x) (myParListEncLinRec (tag+1) strat xs) (fromI# tag)

-- OLD Strats /1
{-
Game.hs:76:38:
    No instance for (Num Done)
      arising from a use of `myparEnc' at Game.hs:76:38-106
    Possible fix: add an instance declaration for (Num Done)
    In the expression:
        myparEnc
          (strat x) (myParListEncLinRec (tag + 1) strat xs) (fromI# tag)
    In the definition of `myParListEncLinRec':
        myParListEncLinRec tag strat (x : xs)
                             = myparEnc
                                 (strat x) (myParListEncLinRec (tag + 1) strat xs) (fromI# tag)
-}

-- TODO update for Eval version of parList
-- usign do natation (bind results)
-- [] = return []
-- do
--  a <- strat x-    
--    b <- myParListEncLinRec (tag+1) strat xs
--    return myparEnc a b (fromI# tag)

-- strategy version
#if defined(STRATEGIES) && defined(EVAL_STRATEGIES)
myParListEncLinRec :: Int -> Strategy a -> Strategy [a]     
myParListEncLinRec  _  strat []     = return []
myParListEncLinRec tag strat (x:xs) = do
    n <- ((myrparEnc tag) `dot` strat) x                           -- (rpar `dot` strat) x      -- creates parallelism (same as evallist); allpies a start to x
    m <- myParListEncLinRec (tag+1) strat xs 
    return (n:m)
--myparEnc (strat x) (myParListEncLinRec (tag+1) strat xs) (fromI# tag)
#endif

------------------------------
-- END parEnc stuff
------------------------------

alternate :: Int -> Int -> Int -> Piece -> Player -> Player -> Board -> [Move]
alternate _ _ _ _ _ _ b | fullBoard b = []
alternate n _ _ _ _ _ b | static n b == XWin = []
alternate n _ _ _ _ _ b | static n b == OWin = []
alternate n depth thresh player f g board = move : alternate n depth thresh opponent g f board'
	where
	move@(board',eval) = best f possibles scores
	scores = map (bestMove n depth thresh opponent g f) possibles   
#if defined(STRATEGIES)
# if defined(EVAL_STRATEGIES)                                                                 
                                                    `using` myParListEncLinRec 20 rseq        -- parListEnc rseq tag -- tag 0 + index -> 0,1,2,3 (index from list length); [0..n] -- top level (kicking off parallelism) -- was: parList rseq --
# elif defined(SM_STRATEGIES)
                                                    `using` parList rwhnf
# else 
                                                    `usingOld` parListEnc 20 rwhnf  -- `using` parList rwhnf
# endif
#endif
	possibles = newPositions player board
        opponent = opposite player

opposite :: Piece -> Piece
opposite X = Board.O
opposite Board.O = X


best :: Player -> [Board] -> [Evaluation] -> Move
best _ [] (s:ss)     = ([], s)                               -- added due to: minimax_enc_P2_O2_pp [PE 1]: Game.hs:(157,0)-(161,37): Non-exhaustive patterns in function best
best _ (b:bs) []     = (b, Score 0) 
best f (b:bs) (s:ss) = best' b s bs ss
	where
	best' b s [] [] = (b,s)
	best' b s (b':bs) (s':ss) | s==(f s s') = best' b s bs ss
				  | otherwise 	= best' b' s' bs ss

showMove :: Move -> String
showMove (b,e) = show e ++ "\n" ++ showBoard b

bestMove :: Int-> Int -> Int -> Piece -> Player -> Player -> Board -> Evaluation
bestMove n depth thresh p f g 
  = parMise thresh f g                                                                             -- originally thresh=2
  . cropTree
  . mapTree (static n)
  . prune depth
  . searchTree p

cropTree :: (Tree Evaluation) -> (Tree Evaluation)
cropTree (Branch a []) = (Branch a [])
cropTree (Branch (Score x) l) = Branch (Score x) (map cropTree l)
cropTree (Branch x l) = Branch x []

searchTree :: Piece -> Board -> (Tree Board)
searchTree p board = repTree (newPositions p) (newPositions (opposite p)) board

mise :: Player -> Player -> (Tree Evaluation) -> Evaluation
mise f g (Branch a []) = a
mise f g (Branch _ l) = foldr f (g OWin XWin) (map (mise g f) l)

parMise :: Int -> Player -> Player -> (Tree Evaluation) -> Evaluation
parMise 0 f g t = mise f g t
parMise n f g (Branch a []) = a
parMise n f g (Branch _ l) = foldr f (g OWin XWin) (map (parMise (n-1) g f) l 
#if defined(STRATEGIES)
# if defined(EVAL_STRATEGIES)
                                                    `using` myParListEncLinRec 0 rseq          -- TODO: tag = 0 (smaller values) as repeated more often
# elif defined(SM_STRATEGIES)
                                                    `using` parList rwhnf
# else 
                                                    `usingOld` parListOld rwhnf
# endif
#endif
                                                    )
