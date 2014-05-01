{-# LANGUAGE BangPatterns #-}

module Main (main) where

import qualified Control.Foldl as Foldl
import           Control.Monad
import           Criterion.Main
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import           Data.Word
import           System.Random (randomIO)

import           Control.Loop (forLoop, numLoop)
import           Control.Loop.Internal (loop, unsafeLoop)


main :: IO ()
main = do

  r <- randomIO

  -- Warning: -fllvm can compile away the `unsafe` loops to complete no-ops.

  defaultMain
    [ bgroup "int" [ bench "loop"                         $ nfIO $ loop                         0 (1000000 :: Int)    (\_ -> return ())
                   , bench "unsafeLoop"                   $ nfIO $ unsafeLoop                   0 (1000000 :: Int)    (\_ -> return ())
                   , bench "numLoop"                      $ nfIO $ numLoop                      0 (1000000 :: Int)    (\_ -> return ())
                   , bench "forLoop"                      $ nfIO $ forLoop                      (0 :: Int) (< 1000000) (+1) (\_ -> return ())
                   , bench "loopLocal"                    $ nfIO $ loopLocal                    0 (1000000 :: Int)    (\_ -> return ())
                   , bench "unsafeLoopLocal"              $ nfIO $ unsafeLoopLocal              0 (1000000 :: Int)    (\_ -> return ())
                   , bench "loopMonad"                    $ nfIO $ loopMonad                    0 (1000000 :: Int)    (\_ -> return ())
                   , bench "numLoopMonadIntInlinable"     $ nfIO $ numLoopMonadIntInlinable     0 (1000000 :: Int)    (\_ -> return ())
                   , bench "numLoopMonadIntInline"        $ nfIO $ numLoopMonadIntInline        0 (1000000 :: Int)    (\_ -> return ())
                   , bench "numLoopInt"                   $ nfIO $ numLoopInt                   0 (1000000 :: Int)    (\_ -> return ())
                   , bench "numLoopEndInt"                $ nfIO $ numLoopEndInt                  (1000000 :: Int)    (\_ -> return ())
                   , bench "listForM_Int"                 $ nfIO $ listForM_Int                 0 (1000000 :: Int)    (\_ -> return ())
                   , bench "vectorFromListInt"            $ nfIO $ vectorFromListInt            0 (1000000 :: Int)    (\_ -> return ())
                   , bench "vectorEnumFromToInt"          $ nfIO $ vectorEnumFromToInt          0 (1000000 :: Int)    (\_ -> return ())
                -- Allocates the vector -> linear space and slow
                -- , bench "vectorEnumFromNInt"           $ nfIO $ vectorEnumFromNInt           0 (1000000 :: Int)    (\_ -> return ())
                   , bench "uvectorFromListInt"           $ nfIO $ uvectorFromListInt           0 (1000000 :: Int)    (\_ -> return ())
                   , bench "uvectorEnumFromToInt"         $ nfIO $ uvectorEnumFromToInt         0 (1000000 :: Int)    (\_ -> return ())
                -- Allocates the vector -> linear space and slow
                -- , bench "uvectorEnumFromNInt"          $ nfIO $ uvectorEnumFromNInt          0 (1000000 :: Int)    (\_ -> return ())
                   , bench "FoldL.FoldM"                  $ nfIO $ Foldl.foldM (Foldl.FoldM (\_m _x -> return ()) (return ()) return) [1..1000000::Int]
                   ]

    , bgroup "w32" [ bench "loop"                          $ nfIO $ loop                        0 (1000000 :: Word32) (\_ -> return ())
                   , bench "unsafeLoop"                    $ nfIO $ unsafeLoop                  0 (1000000 :: Word32) (\_ -> return ())
                   , bench "numLoop"                       $ nfIO $ numLoop                     0 (1000000 :: Word32) (\_ -> return ())
                   , bench "forLoop"                       $ nfIO $ forLoop                     (0 :: Word32) (< 1000000) (+1) (\_ -> return ())
                   , bench "loopLocal"                     $ nfIO $ loopLocal                   0 (1000000 :: Word32) (\_ -> return ())
                   , bench "unsafeLoopLocal"               $ nfIO $ unsafeLoopLocal             0 (1000000 :: Word32) (\_ -> return ())
                   , bench "loopMonad"                     $ nfIO $ loopMonad                   0 (1000000 :: Word32) (\_ -> return ())
                   , bench "numLoopW32"                    $ nfIO $ numLoopW32                  0 (1000000 :: Word32) (\_ -> return ())
                   , bench "numLoopEndW32"                 $ nfIO $ numLoopEndW32                 (1000000 :: Word32) (\_ -> return ())
                   , bench "listForM_W32"                  $ nfIO $ listForM_W32                0 (1000000 :: Word32) (\_ -> return ())
                   , bench "vectorFromListW32"             $ nfIO $ vectorFromListW32           0 (1000000 :: Word32) (\_ -> return ())
                   , bench "vectorEnumFromToW32"           $ nfIO $ vectorEnumFromToW32         0 (1000000 :: Word32) (\_ -> return ())
                   , bench "uvectorFromListW32"            $ nfIO $ uvectorFromListW32          0 (1000000 :: Word32) (\_ -> return ())
                   , bench "uvectorEnumFromToW32"          $ nfIO $ uvectorEnumFromToW32        0 (1000000 :: Word32) (\_ -> return ())
                   , bench "FoldL.FoldM"                   $ nfIO $ Foldl.foldM (Foldl.FoldM (\_m _x -> return ()) (return ()) return) [1..1000000::Word32]
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


-- Same as `numLoop` just completely monomorphic, marked as INLINEABLE.
numLoopMonadIntInlinable :: Int -> Int -> (Int -> IO ()) -> IO ()
numLoopMonadIntInlinable start end f = go start
  where
    go !x | x == end  = f x
          | otherwise = f x >> go (x+1)

{-# INLINEABLE numLoopMonadIntInlinable #-}


-- Same as `numLoop` just completely monomorphic, marked as INLINE.
numLoopMonadIntInline :: Int -> Int -> (Int -> IO ()) -> IO ()
numLoopMonadIntInline start end f = go start
  where
    go !x | x == end  = f x
          | otherwise = f x >> go (x+1)

{-# INLINE numLoopMonadIntInline #-}


-- Same as `numLoop`, specialized to Int.
numLoopInt :: (Monad m) => Int -> Int -> (Int -> m ()) -> m ()
numLoopInt start end f = go start
  where
    go !n | n == end  = f n >> return ()
          | otherwise = f n >> go (n+1)

{-# INLINEABLE numLoopInt #-}


-- Same as `numLoop`, specialized to Int with constant start value.
numLoopEndInt :: (Monad m) => Int -> (Int -> m ()) -> m ()
numLoopEndInt end f = go 0
  where
    go !n | n == end  = f n >> return ()
          | otherwise = f n >> go (n+1)

{-# INLINEABLE numLoopEndInt #-}


-- Same as `numLoop`, specialized to Int32.
numLoopW32 :: (Monad m) => Word32 -> Word32 -> (Word32 -> m ()) -> m ()
numLoopW32 start end f = go start
  where
    go !n | n == end  = f n >> return ()
          | otherwise = f n >> go (n+1)

{-# INLINEABLE numLoopW32 #-}


-- Same as `numLoop`, specialized to Int32 with constant start value.
numLoopEndW32 :: (Monad m) => Word32 -> (Word32 -> m ()) -> m ()
numLoopEndW32 end f = go 0
  where
    go !n | n == end  = f n >> return ()
          | otherwise = f n >> go (n+1)

{-# INLINEABLE numLoopEndW32 #-}


-- Using `forM_`, specialized to Int.
listForM_Int :: (Monad m) => Int -> Int -> (Int -> m ()) -> m ()
listForM_Int start end f = forM_ [start..end] f


-- Using `V.forM_` with `V.fromList`, specialized to Int.
vectorFromListInt :: (Monad m) => Int -> Int -> (Int -> m ()) -> m ()
vectorFromListInt start end f = V.forM_ (V.fromList [start..end]) f


-- Using `V.forM_` with `V.enumFromTo`, specialized to Int.
vectorEnumFromToInt :: (Monad m) => Int -> Int -> (Int -> m ()) -> m ()
vectorEnumFromToInt start end f = V.forM_ (V.enumFromTo start end) f


-- Using `V.forM_` with `V.enumFromN`, specialized to Int.
vectorEnumFromNInt :: (Monad m) => Int -> Int -> (Int -> m ()) -> m ()
vectorEnumFromNInt start end f = V.forM_ (V.enumFromN start (end - start + 1)) f


-- Using `U.forM_` with `U.fromList`, specialized to Int.
uvectorFromListInt :: (Monad m) => Int -> Int -> (Int -> m ()) -> m ()
uvectorFromListInt start end f = U.forM_ (U.fromList [start..end]) f


-- Using `U.forM_` with `U.enumFromTo`, specialized to Int.
uvectorEnumFromToInt :: (Monad m) => Int -> Int -> (Int -> m ()) -> m ()
uvectorEnumFromToInt start end f = U.forM_ (U.enumFromTo start end) f


-- Using `U.forM_` with `U.enumFromN`, specialized to Int.
uvectorEnumFromNInt :: (Monad m) => Int -> Int -> (Int -> m ()) -> m ()
uvectorEnumFromNInt start end f = U.forM_ (U.enumFromN start (end - start + 1)) f


-- Using `forM_`, specialized to Word32.
listForM_W32 :: (Monad m) => Word32 -> Word32 -> (Word32 -> m ()) -> m ()
listForM_W32 start end f = forM_ [start..end] f


-- Using `V.forM_` with `V.fromList`, specialized to Word32.
vectorFromListW32 :: (Monad m) => Word32 -> Word32 -> (Word32 -> m ()) -> m ()
vectorFromListW32 start end f = V.forM_ (V.fromList [start..end]) f


-- Using `V.forM_` with `V.enumFromTo`, specialized to Word32.
vectorEnumFromToW32 :: (Monad m) => Word32 -> Word32 -> (Word32 -> m ()) -> m ()
vectorEnumFromToW32 start end f = V.forM_ (V.enumFromTo start end) f


-- Using `U.forM_` with `U.fromList`, specialized to Word32.
uvectorFromListW32 :: (Monad m) => Word32 -> Word32 -> (Word32 -> m ()) -> m ()
uvectorFromListW32 start end f = U.forM_ (U.fromList [start..end]) f


-- Using `U.forM_` with `U.enumFromTo`, specialized to Word32.
uvectorEnumFromToW32 :: (Monad m) => Word32 -> Word32 -> (Word32 -> m ()) -> m ()
uvectorEnumFromToW32 start end f = U.forM_ (U.enumFromTo start end) f
