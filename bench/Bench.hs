{-# LANGUAGE BangPatterns #-}

module Main (main) where

import           Criterion.Main
import           Data.Word
import           System.Random (randomIO)

import           Control.Loop (loop, unsafeLoop)


main :: IO ()
main = do

  r <- randomIO

  -- Warning: -fllvm can compile away the `unsafe` loops to complete no-ops.

  defaultMain
    [ bgroup "int" [ bench "loop"                         $ nfIO $ loop                         0 (1000000 :: Int)    (\_ -> return ())
                   , bench "unsafeLoop"                   $ nfIO $ unsafeLoop                   0 (1000000 :: Int)    (\_ -> return ())
                   , bench "loopLocal"                    $ nfIO $ loopLocal                    0 (1000000 :: Int)    (\_ -> return ())
                   , bench "unsafeLoopLocal"              $ nfIO $ unsafeLoopLocal              0 (1000000 :: Int)    (\_ -> return ())
                   , bench "loopMonad"                    $ nfIO $ loopMonad                    0 (1000000 :: Int)    (\_ -> return ())
                   , bench "unsafeLoopMonadIntInlinable"  $ nfIO $ unsafeLoopMonadIntInlinable  0 (1000000 :: Int)    (\_ -> return ())
                   , bench "unsafeLoopMonadIntInline"     $ nfIO $ unsafeLoopMonadIntInline     0 (1000000 :: Int)    (\_ -> return ())
                   , bench "unsafeLoopInt"                $ nfIO $ unsafeLoopInt                0 (1000000 :: Int)    (\_ -> return ())
                   , bench "unsafeLoopEndInt"             $ nfIO $ unsafeLoopEndInt               (1000000 :: Int)    (\_ -> return ())
                   ]

    , bgroup "w32" [ bench "loop"                          $ nfIO $ loop                        0 (1000000 :: Word32) (\_ -> return ())
                   , bench "unsafeLoop"                    $ nfIO $ unsafeLoop                  0 (1000000 :: Word32) (\_ -> return ())
                   , bench "loopLocal"                     $ nfIO $ loopLocal                   0 (1000000 :: Word32) (\_ -> return ())
                   , bench "unsafeLoopLocal"               $ nfIO $ unsafeLoopLocal             0 (1000000 :: Word32) (\_ -> return ())
                   , bench "loopMonad"                     $ nfIO $ loopMonad                   0 (1000000 :: Word32) (\_ -> return ())
                   , bench "unsafeLoopW32"                 $ nfIO $ unsafeLoopW32               0 (1000000 :: Word32) (\_ -> return ())
                   , bench "unsafeLoopEndW32"              $ nfIO $ unsafeLoopEndW32              (1000000 :: Word32) (\_ -> return ())
                   ]

    -- `succ` is almost twice as slow as (+1) because it does an overflow check.
    -- This doesn't become apparent in this benchmark, only if you use it in a loop.
    , bgroup "inc" [ bench "+1"   $ whnf (+1) (r :: Int)
                   , bench "succ" $ whnf succ (r :: Int)
                   ]
    ]


-- Same as `loop` just defined locally.
loopLocal :: (Enum e, Eq e, Monad m) => e -> e -> (e -> m ()) -> m ()
loopLocal start end f = go start
  where
    go !x | x == end  = f x
          | otherwise = f x >> go (succ x)

{-# INLINEABLE loopLocal #-}


-- Same as `unsafeLoop` just defined locally.
unsafeLoopLocal :: (Enum e, Eq e, Monad m) => e -> e -> (e -> m ()) -> m ()
unsafeLoopLocal start end f = go start
  where
    go !x | x == end  = f x
          | otherwise = f x >> go (unsafeSucc x)
    unsafeSucc = toEnum . (+ 1) . fromEnum

{-# INLINEABLE unsafeLoopLocal #-}


-- Same as `loop` just with the monad monomorphic.
loopMonad :: (Enum e, Eq e) => e -> e -> (e -> IO ()) -> IO ()
loopMonad start end f = go start
  where
    go !x | x == end  = f x
          | otherwise = f x >> go (succ x)

{-# INLINEABLE loopMonad #-}


-- Same as `unsafeLoop` just completely monomorphic, marked as INLINEABLE.
unsafeLoopMonadIntInlinable :: Int -> Int -> (Int -> IO ()) -> IO ()
unsafeLoopMonadIntInlinable start end f = go start
  where
    go !x | x == end  = f x
          | otherwise = f x >> go (x + 1)

{-# INLINEABLE unsafeLoopMonadIntInlinable #-}


-- Same as `unsafeLoop` just completely monomorphic, marked as INLINE.
unsafeLoopMonadIntInline :: Int -> Int -> (Int -> IO ()) -> IO ()
unsafeLoopMonadIntInline start end f = go start
  where
    go !x | x == end  = f x
          | otherwise = f x >> go (x + 1)

{-# INLINE unsafeLoopMonadIntInline #-}


-- Same as `unsafeLoop`, specialized to Int.
unsafeLoopInt :: (Monad m) => Int -> Int -> (Int -> m ()) -> m ()
unsafeLoopInt start end f = go start
  where
    go !n | n == end  = f n >> return ()
          | otherwise = f n >> go (n+1)

{-# INLINEABLE unsafeLoopInt #-}


-- Same as `unsafeLoop`, specialized to Int with constant start value.
unsafeLoopEndInt :: (Monad m) => Int -> (Int -> m ()) -> m ()
unsafeLoopEndInt end f = go 0
  where
    go !n | n == end  = f n >> return ()
          | otherwise = f n >> go (n+1)

{-# INLINEABLE unsafeLoopEndInt #-}


-- Same as `unsafeLoop`, specialized to Int32.
unsafeLoopW32 :: (Monad m) => Word32 -> Word32 -> (Word32 -> m ()) -> m ()
unsafeLoopW32 start end f = go start
  where
    go !n | n == end  = f n >> return ()
          | otherwise = f n >> go (n+1)

{-# INLINEABLE unsafeLoopW32 #-}


-- Same as `unsafeLoop`, specialized to Int32 with constant start value.
unsafeLoopEndW32 :: (Monad m) => Word32 -> (Word32 -> m ()) -> m ()
unsafeLoopEndW32 end f = go 0
  where
    go !n | n == end  = f n >> return ()
          | otherwise = f n >> go (n+1)

{-# INLINEABLE unsafeLoopEndW32 #-}
