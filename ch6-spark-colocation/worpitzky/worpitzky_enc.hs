{-# OPTIONS -cpp -fglasgow-exts #-}
{-
    Check the Worpitzky identity (Concrete Maths, p255)
                    
     x^n = \Sum_k  <n> (x+k)
                    k    n

                  euler  binomials                            
-} 

module Main(main) where

import System(getArgs)
import Debug.Trace(trace)
import Control.Monad(unless)
-- import Control.Parallel(par)
import GHC.Base --(par#, parGlobal#, Int#, parEnc#)
import GHC.Conc --(par, pseq, I#)                   


-- helper function: create and unboxed Int from Int
{-# INLINE fromI# #-}
fromI# :: (Integral n) => n -> Int#
fromI# n = n#  where !(I# n#) = fromIntegral n

-- a -> Int# -> Int# -> Int# -> Int# -> b -> Int#
-- parGlobal :: a -> b -> b
-- parGlobal x y = case (parGlobal# x 0# 0# 0# 0# y) of { i' -> y }

mypar :: a -> b -> b
mypar x y = case (par# x) of { i' -> y }

-- a -> Int# -> Int# -> Int# -> Int# -> b -> Int#
myparGlobal :: Num b => a -> b -> b
myparGlobal x y = case (parGlobal# x 0# 0# 0# 0# y) of { i' -> y }

--myparEnc :: a -> b -> Int# -> b
myparEnc x y e = case (parEnc# x e 2# y) of {i' -> if (I# i')==0 then 1 else y}  -- base 2


main = do args <- getArgs
          unless (length args == 3) $
            error "Usage: worpitzky <x> <n> <t>  ... testing Worpitzky identity for x^n, using t as a threshold"
          let 
            x :: Integer
            x = read (args!!0)  -- 
            n :: Integer
            n = read (args!!1)  -- 
            t :: Integer
            t = read (args!!2)  -- threshold
            res = worpitzky x n t
          putStrLn ("Testing Worpitzky's identity for x^n ..." ++ (show x) ++ " " ++ (show n) ++ " " ++ (show t) ++ " = " ++ (show res))

worpitzky ::  Integer -> Integer -> Integer -> Bool
worpitzky x n t = x^n == sum [ (par_euler_enc n k t 0#) * (bin (x+k) n) | k <- [0,1..n] ]

-- euler numbers (Concrete Maths, p254)
euler :: Integer -> Integer -> Integer
euler 0 k = if k==0 then 1 else 0
euler n k = (k+1) * (euler (n-1) k) + (n-k)*(euler (n-1) (k-1))

-- binomials 
bin :: Integer -> Integer -> Integer
bin n k = fact n `div` ( (fact k) * (fact (n-k)) )

fact :: Integer -> Integer
fact 0 = 1
fact n = product [1..n]

par_euler :: Integer -> Integer -> Integer -> Integer
par_euler 0 k t = if k==0 then 1 else 0
par_euler n k t | n<t = euler n k
par_euler n k t = e2 `mypar` (e1 `pseq` ((k+1) * e1 + (n-k)*e2))
                  where e1 = par_euler (n-1) k t
                        e2 = par_euler (n-1) (k-1) t

--  encoding: 0 = left subtree, 1 = right subtree
par_euler_enc :: Integer -> Integer -> Integer -> Int# -> Integer
par_euler_enc 0 k t e = if k==0 then 1 else 0
par_euler_enc n k t _ | n<t = euler n k
par_euler_enc n k t e = (myparEnc e1 e2 e) `pseq` ((k+1) * e1 + (n-k)*e2)
                      where e1 = par_euler_enc (n-1)  k    t 0#
                            e2 = par_euler_enc (n-1) (k-1) t 1#


{-

stir1 :: Integer -> Integer -> Integer
stir1 _ 0 = 1
stir1 0 _ = 1
stir1 n k = (n-1)*(stir1 (n-1) k)+(stir1 (n-1) (k-1))

par_stir1 :: Integer -> Integer -> Integer -> Integer
par_stir1 n k t | n<t || k<t = stir1 n k
                | otherwise  = s1 `par` (s2 `par` (n-1)*s1+s2)
                	       where s1 = stir1 (n-1) k
                	             s2 = stir1 (n-1) (k-1)

stir2 :: Integer -> Integer -> Integer
stir2 _ 1 = 1
stir2 0 _ = 1
stir2 n k = (stir2 (n-1) (k-1)) + k*(stir2 (n-1) k)

par_stir2 :: Integer -> Integer -> Integer -> Integer
par_stir2 n k t | n<t || k<t = stir2 n k
                | otherwise  = s1 `mypar` (s2 `pseq` s1+k*s2)
                	       where s1 = par_stir2 (n-1) (k-1) t
                	             s2 = k*(par_stir2 (n-1) k t)
-}

