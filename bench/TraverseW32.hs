{-# LANGUAGE BangPatterns #-}

module Main (main) where

import           Criterion.Main
import           Data.Word

import           Control.Loop (forLoop)
import           Control.Loop.Internal (loop, unsafeLoop, numLoop)


main :: IO ()
main = do

  -- Warning: -fllvm can compile away the `unsafe` loops to complete no-ops.

  defaultMain
    [ bgroup "traversew32" [ bench "loop"        $ nfIO $ loop        0 (maxBound :: Word32) (\_ -> return ())
                           , bench "unsafeLoop"  $ nfIO $ unsafeLoop  0 (maxBound :: Word32) (\_ -> return ())
                           , bench "numLoop"     $ nfIO $ numLoop     0 (maxBound :: Word32) (\_ -> return ())
                           , bench "forLoop"     $ nfIO $ forLoop     0 (< (maxBound :: Word32)) (+1) (\_ -> return ())
                           ]
    ]
