{-# OPTIONS -fglasgow-exts #-}
-- Time-stamp: <Tue Feb 08 2011 00:09:22 Stardate: Stardate: [-28]4414.82 hwloidl>
--    updated:      Mar 03 2015 (eb120@hw.ac.uk) to use parEnc primOp
-- 
-- Euler totient function.
-- Used in "Research Directions in Parallel Functional Programming",
-- Chapter "Performance Monitoring", Nathan Charles and Colin Runciman.
--
-- compile with: -XBangPatterns
---------------------------------------------------------------------------

module Main(main) where

import Data.List(transpose)
import System(getArgs)
-- import Control.Parallel(par)
import GHC.Base --(par#, parGlobal#, parEnc#, Int#)
import GHC.Conc --(par,pseq)
import Control.Monad

{-# INLINE fromI# #-}
fromI# :: (Integral n) => n -> Int#
fromI# n = n#  where !(I# n#) = fromIntegral n

-- mypar :: a -> b -> b
-- mypar x y = case (par# x) of { i' -> y }

myparEnc :: a -> b -> Int# -> Int# -> b
myparEnc x y e b = case (parEnc# x b e y) of {i' -> y}  -- base b, encoding e

main = do args <- getArgs
          unless (length args == 2) $
            error "Usage: sE <n> <c>\n  where <n> is the upper bound of the interval and <c> is the chunk size" 
          let 
            n = read (args!!0) :: Int -- size of the interval
            c = read (args!!1) :: Int -- chunk size
            -- if ((n-m) `div` c + 1) < 1 || ((n-m) `div` c + 1) > 15 then error "ERROR: n / c = base should be within range [2..15]" else ()
            res = sumEuler 1 n c 0#
          -- putStrLn ("sumEuler [1.." ++ (show n) ++ "] with chunk size " ++ (show c) ++ " = " ++ (show res))
          putStrLn ("sumEuler over an interval from 1 to " ++ (show n) ++ " with chunk size " ++ (show c) ++ " = " ++ (show res))
-- sumEuler over an interval from m to n (with chunk size c)
sumEuler ::  Int -> Int -> Int -> Int# -> Int
sumEuler m n c e | m>n = 0 
	         | otherwise = let 
                                this = sum (map euler [m..m+c-1])
                                rest = sumEuler (min (m+c) (n+1)) n c (e +# 1#)         
                               in
                                (myparEnc this rest e (fromI# ((n-m) `div` c + 1)) ) `pseq` this+rest
                                --this `par` (rest `pseq` this+rest)
{-
  let 
    eulerList = let
  		    numbers = [1..n]
                    blocks = chunk c numbers
                    -- blocks = shuffle (length numbers `div` c) numbers
                in	
  	            numbers `par` (map (sum . map euler) blocks)
  in	      
    (parList eulerList) `pseq` (sum eulerList)
-}

sumEuler_seq :: Int -> Int
sumEuler_seq = sum . map euler . enumFromTo 1

euler :: Int -> Int
euler n = let
            relPrimes = let
                          numbers = [1..(n-1)]
                        in
                          {- numbers `par` -} (filter (relprime n) numbers)
          in
            {- (spine relPrimes) `par`-} (length relPrimes)

-- aux fcts
hcf :: Int -> Int -> Int
hcf x 0 = x
hcf x y = hcf y (rem x y)

relprime :: Int -> Int -> Bool
relprime x y = hcf x y == 1

-- strategic functions (could uses Strategies module instead)
parList :: [Int] -> ()
parList = foldr par ()

spine :: [Int] -> ()
spine []     = ()
spine (_:xs) = spine xs

chunk :: Int -> [a] -> [[a]]
chunk _n [] = []
chunk n  xs = ys:chunk n zs where (ys,zs) = splitAt n xs

slice :: Int ->  [a] -> [[a]]
slice n = transpose . chunk n
