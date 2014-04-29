{-# LANGUAGE BangPatterns #-}

module Main (main) where

import           Control.Monad.State.Strict
import           Criterion.Main
import           Data.List (foldl')
import           Data.IORef
import           Data.Word


main :: IO ()
main = do

  -- Warning: -fllvm can compile away the `unsafe` loops to complete no-ops.

  defaultMain
    [ bgroup "sum32"       [ bench "sumW32loopIORef"     $ nfIO (sumW32loopIORef                          1000000)
                           , bench "sumW32StrictState"   $ whnf (\n -> execState (sumW32StrictState n) 0) 1000000
                           , bench "foldlW32"            $ whnf (\n -> foldl' (+) 0 [0..n::Word32])       1000000
                           , bench "sumIntloopIORef"     $ nfIO (sumIntloopIORef                          1000000)
                           , bench "sumIntStrictState"   $ whnf (\n -> execState (sumIntStrictState n) 0) 1000000
                           , bench "foldlInt"            $ whnf (\n -> foldl' (+) 0 [0..n::Int])          1000000
                           ]
    ]



forLoop :: (Monad m) => a -> (a -> Bool) -> (a -> a) -> (a -> m ()) -> m ()
forLoop start cond inc f = go start
  where
    go !x | cond x    = f x >> go (inc x)
          | otherwise = return ()

{-# INLINE forLoop #-}


sumW32loopIORef :: Word32 -> IO Word32
sumW32loopIORef n = do
  ref <- newIORef 0
  forLoop (0 :: Word32) (< n) (+1) $ \i -> do
    modifyIORef' ref (+i)
  readIORef ref


sumW32StrictState :: Word32 -> State Word32 ()
sumW32StrictState n = do
  forLoop (0 :: Word32) (< n) (+1) $ \i -> do
    x <- get
    put $! x + i


sumIntloopIORef :: Int -> IO Int
sumIntloopIORef n = do
  ref <- newIORef 0
  forLoop (0 :: Int) (< n) (+1) $ \i -> do
    modifyIORef' ref (+i)
  readIORef ref


sumIntStrictState :: Int -> State Int ()
sumIntStrictState n = do
  forLoop (0 :: Int) (< n) (+1) $ \i -> do
    x <- get
    put $! x + i
