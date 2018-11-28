{-# OPTIONS -fglasgow-exts #-}
-- -*- haskell -*-
-- Time-stamp: <Thu Oct 14 2010 00:17:39 Stardate: Stardate: [-28]3829.64 hwloidl>
--
-- parfib with a threshold to tune granularity.
-- Haskell98 version.
-----------------------------------------------------------------------------

module Main(main) where

import System(getArgs)

-- import Control.Parallel(par)
import GHC.Base --(par#, parGlobal#, Int#, parEnc#)
import GHC.Conc --(par,pseq, I#)

{-# INLINE fromI# #-}
fromI# :: (Integral n) => n -> Int#
fromI# n = n#  where !(I# n#) = fromIntegral n

mypar :: a -> b -> b
mypar x y = case (par# x) of { i' -> y }

-- a -> Int# -> Int# -> Int# -> Int# -> b -> Int#
myparGlobal :: Num b => a -> b -> b
myparGlobal x y = case (parGlobal# x 0# 0# 0# 0# y) of { i' -> y }

--myparEnc :: a -> b -> Int# -> b
myparEnc x y e = case (parEnc# x e 2# y) of {i' -> if (I# i')==0 then 1 else y}  -- base 2}  -- base 2


main = do args <- getArgs
          let 
            n = read (args!!0)  -- size of the interval
            t = read (args!!1)  -- threshold
            res = pfib n t 0#
          putStrLn ("pfib " ++ (show n) ++ " " ++ (show t) ++ " = " ++ (show res))

-- takes extra encoding symbol 0 = L, 1 = R
pfib :: Integer -> Integer -> Int# -> Integer
pfib 0 _ _ = 1
pfib 1 _ _ = 1
pfib n t a | n <= t    = nfib n  -- sequential call
           | otherwise = (myparEnc x y a) `pseq` x + y + 1
             where 
               x = pfib (n-1) t 0# 
               y = pfib (n-2) t 1#

nfib :: Integer -> Integer
nfib 0 = 1
nfib 1 = 1
nfib n = nfib (n-1) + nfib (n-2) + 1
