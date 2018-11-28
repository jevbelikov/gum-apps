{-# LANGUAGE CPP #-}
module Tree where

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


data Tree a = Branch a [Tree a] deriving Show

repTree :: (a->[a]) -> (a->[a])-> a -> (Tree a)
repTree f g a = Branch a (map (repTree g f) (f a))

#define SEQ

#ifndef SEQ

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Branch a l) 
   = fa `par` Branch fa (map (mapTree f) l `using` myParList)
   where fa = f a

#else  
{- SEQ -}

mapTree :: (a -> b) -> (Tree a) -> (Tree b)
mapTree f (Branch a l) = Branch (f a) (map (mapTree f) l)

#endif

#ifndef SEQ
myParList [] = ()
myParList (x:xs) = x `par` myParList xs

mySeqList [] = ()
mySeqList (x:xs) = x `seq` mySeqList xs

parTree :: Int -> Tree a -> ()
parTree 0 (Branch a xs) = ()
parTree n (Branch a xs) = a `par` mySeqList (map (parTree (n-1)) xs)
#endif

prune :: Int -> (Tree a) -> (Tree a)
prune 0 (Branch a l) = Branch a []
prune (n+1) (Branch a l) = Branch a (map (prune n) l)

