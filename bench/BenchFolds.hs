{-# LANGUAGE BangPatterns #-}

module Main (main) where

import           Control.Monad.State.Strict
import           Criterion.Main

import           Control.Loop (forLoop, forLoopFold, numLoopFold)


main :: IO ()
main = do

  defaultMain
    [ bgroup "pure"    [ bench "sumFold"           $ whnf sumFold          1000000
                       , bench "sumFoldTooStrict"  $ whnf sumFoldTooStrict 1000000
                       , bench "numSumFold"        $ whnf numSumFold       1000000
                       ]
    , bgroup "monadic" [ bench "sumFoldMonadic"    $ whnf sumFoldMonadic   1000000
                       ]
    ]


-- This is too strict if you want to write something like this;
--   forLoopFold  0 (<4) (+1) (error "default should not be evaluated") (\acc x -> x)
-- Namely it evaluates acc0 even if it is never used.
-- See http://neilmitchell.blogspot.co.uk/2013/08/destroying-performance-with-strictness.html
forLoopFoldTooStrict :: a -> (a -> Bool) -> (a -> a) -> acc -> (acc -> a -> acc) -> acc
forLoopFoldTooStrict start cond inc acc0 f = go acc0 start
  where
    go !acc !x | cond x    = go (f acc x) (inc x)
               | otherwise = acc

{-# INLINE forLoopFoldTooStrict #-}


sumFold :: Int -> Int
sumFold n = forLoopFold 0 (< n) (+1) 0 (+)

sumFoldTooStrict :: Int -> Int
sumFoldTooStrict n = forLoopFoldTooStrict 0 (< n) (+1) 0 (+)

sumFoldMonadic :: Int -> Int
sumFoldMonadic n = flip execState 0 $ do
  forLoop 0 (< n) (+1) $ \i -> do
    x <- get
    put $! x + i

numSumFold :: Int -> Int
numSumFold n = numLoopFold 0 (n - 1) 0 (+) -- numLoopFold is inclusive
