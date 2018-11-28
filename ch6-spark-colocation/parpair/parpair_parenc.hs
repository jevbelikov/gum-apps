{-
 parpair_parenc.hs      eb120@hw.ac.uk  init:18/08/2015
                                        updt:13/10/2015
                     
 a constrcuted program to investigate the use of the spark colocation policy (+RTS -qy)
 at top level elements of a tuple are evaluated in parallel, each containing 
 a different parallel computation (parfact, parfib); the sparks from a computation are 
 co-located with other sparks from that computation 

 based on parfact and parfib from the 'Seq No More paper' (Haskell98 versions)
-}

{-# OPTIONS -fglasgow-exts #-}

module Main(main) where

import System(getArgs)
import GHC.Base --(par#, parGlobal#, Int#, parEnc#)
import GHC.Conc --(par, pseq, I#)                   
import Control.Monad(unless)
-- import Control.Parallel(par)


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

-- sequential: return number of function calls to calculate n'th Fibonacci number
nfib :: Integer -> Integer
nfib 0 = 1
nfib 1 = 1
nfib n = nfib (n-1) + nfib (n-2) + 1

-- parfib with a threshold to tune granularity
-- takes extra encoding symbol 0 = L, 1 = R                      TODO just L or Land R for each function ??!
pfib :: Integer -> Integer -> Int# -> Integer
pfib 0 _ _ = 1
pfib 1 _ _ = 1
pfib n t a | n <= t    = nfib n         -- sequential call
           | otherwise = (myparEnc x y a) `pseq` x + y + 1
             where 
               x = pfib (n-1) t 3# --0# 
               y = pfib (n-2) t 3# --0#


-- parfact (d&c) with a threshold to tune granularity
-- Uses pairs as argument to test transmission on composed data structures
-- Uses expensive multiplication to do some real work (over lists)

fact :: Integer -> Integer -> Integer
fact m n | m >  n    = 1
         | m == n    = m
         | otherwise = l*r
                       where m' = (m+n) `div` 2
                             l = fact m m'
                             r = fact (m'+1) n

-- divide-and-conquer 
parfact :: (Integer, Integer) -> Integer -> Integer
parfact (m, n) t | (n-m)<=t  = product [m..n]                   -- fact m n
                 | otherwise = l `par` (r `par` (m' `seq` l*r)) -- (l `mult` r))) -- l*r         
                               where m' = (m+n) `div` 2         -- (m `plus` n) `div` 2  -- (m+n) `div` 2
                             	     l = parfact (m, m') t
                             	     r = parfact ((m'+1), n) t

pfact :: (Integer, Integer) -> Integer -> Int# -> Integer
pfact (m, n) t a | (n-m) <= t = product [m..n]                     -- fact m n
                 | otherwise  = (myparEnc l (myparEnc r (m' `seq` l*r) 2#) 2#)    -- (l `mult` r))) -- l*r -- base 2  ?? seq outside ??
                                where m' = (m+n) `div` 2         -- (m `plus` n) `div` 2  -- (m+n) `div` 2
                             	      l = pfact (m, m')     t 1#
                             	      r = pfact ((m'+1), n) t 1#

-- parpair :: ((Integer, Integer) -> Integer -> Int# -> Integer) -> Integer -> Integer -> Integer -> (Integer -> Integer -> Int# -> Integer) -> Integer -> Integer -> (Integer, Integer)
parpair :: (Integer -> Integer -> Integer -> Int# -> Integer) -> Integer -> Integer -> Integer -> (Integer -> Integer -> Int# -> Integer) -> Integer -> Integer -> (Integer, Integer)
-- parpair pfact_f m n t pfib_f k u = (myparEnc pfa pfb 0#) `pseq` (pfa, pfb)
parpair parSE_f m n t pfib_f k u = (myparEnc pfa pfb 0#) `pseq` (pfa, pfb)
  where 
    --pfa = pfact_f (m,n) t 0#
    pfa = parSE_f m n t 4#
    pfb = pfib_f  k u 1#


pse :: Integer -> Integer -> Integer -> Integer
pse l u t = parSumEuler l u t 4#

-- an unbalanced divide-and-conquer-based version
parSumEuler :: Integer -> Integer -> Integer -> Int# -> Integer
parSumEuler lower upper threshold encoding 
  | threshold <= 0 = seqSumEuler lower upper
  | otherwise      = (myparEnc x y encoding) `pseq` x + y
      where 
           x = parSumEuler lower     mid   (threshold-1) 4#      --0#      -- left  sub-tree 
           y = parSumEuler (mid + 1) upper (threshold-1) 4#      --1#      -- right sub-tree
           mid = ((lower+upper) `div` 2)   -- (!) both encodings = 4#  indicating parSumEuler

-- sum of euler for a given interval
seqSumEuler :: Integer -> Integer -> Integer
seqSumEuler l u = sum $ map euler [l,(l+1)..u]

-- number of number < n that are relatively prime to n
euler :: Integer -> Integer
euler n = toInteger $ length $ filter (relprime n) [1..(n-1)]

-- determines whether two numbers are relatively prime
relprime :: Integer -> Integer -> Bool
relprime x y = hcf x y == 1

-- highest common factor (gcd)
hcf :: Integer -> Integer -> Integer
hcf x 0 = x
hcf x y = hcf y (rem x y)

main :: IO ()
main = do args <- getArgs
          unless (length args == 5) $
            error "usage: ./parpair <sumeuler-lower> <sumeuler-upper> <sumeuelr-threshold> <pfib-val> <pfib-threshold>\n"
          let 
            m = read (args!!0)  -- lower     for fact
            n = read (args!!1)  -- upper     for fact
            t = read (args!!2)  -- threshold for fact
            k = read (args!!3)  -- param     for fib
            u = read (args!!4)  -- threshold for fib
            -- res = parpair pfact m n t pfib k u                              -- pfib n t 0#   vs  parfact (1, n) t
            res = parpair parSumEuler m n t pfib k u 
          -- putStr   $ "parpair (parfact (" ++ (show m) ++ ", " ++ (show n) ++ ") "++ show t ++", "
          putStr   $ "parpair (parSumEuler (" ++ (show m) ++ ", " ++ (show n) ++ ") "++ show t ++", "
          putStr   $ "parfib " ++ (show k) ++ " " ++ show u ++  ") = "
          putStrLn $ "(" ++ show (fst res) ++ ", " ++ show (snd res) ++ ")"

