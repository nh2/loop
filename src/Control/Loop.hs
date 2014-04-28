{-# LANGUAGE BangPatterns #-}

-- | Provides a convenient and fast alternative to the common
-- `forM_ [1..n]` idiom, which in many cases GHC cannot fuse to efficient
-- code.
module Control.Loop
  ( loop
  , unsafeLoop
  ) where

import Control.Monad.ST -- For SPECIALIZE only
import Data.Int         -- For SPECIALIZE only
import Data.Word        -- For SPECIALIZE only


-- | @loop start end f@: Loops from @start@ to @end@ (inclusive), executing @f@
-- on each iteration. Same as @forM_ [start..end] f@.
--
-- Uses `succ` inside, which does a bounds (overflow) check.
loop :: (Enum e, Eq e, Monad m) => e -> e -> (e -> m ()) -> m ()
loop start end f = go start
  where
    go !x | x == end  = f x
          | otherwise = f x >> go (succ x)

{-# INLINEABLE loop #-}


-- | Like `loop`, but without bounds (overflow) check.
--
-- This circumvents the implementation of `succ` for the @Enum@ type
-- and uses @toEnum . (+ 1) . fromEnum@ instead, so it will break
-- on Enums that are not contiguous.
unsafeLoop :: (Enum e, Eq e, Monad m) => e -> e -> (e -> m ()) -> m ()
unsafeLoop start end f = go start
  where
    go !x | x == end  = f x
          | otherwise = f x >> go (unsafeSucc x)
    unsafeSucc = toEnum . (+ 1) . fromEnum

{-# INLINEABLE unsafeLoop #-}


-- SPECIALIZE for the monad
{-# SPECIALIZE loop :: (Enum e, Eq e) => e -> e -> (e -> IO ()) -> IO () #-}
{-# SPECIALIZE loop :: (Enum e, Eq e) => e -> e -> (e -> ST s ()) -> ST s () #-}

-- SPECIALIZE for what we iterate over
{-# SPECIALISE loop :: (Monad m) => Bool     -> Bool     -> (Bool     -> m ()) -> m () #-}
{-# SPECIALISE loop :: (Monad m) => Char     -> Char     -> (Char     -> m ()) -> m () #-}
{-# SPECIALISE loop :: (Monad m) => Double   -> Double   -> (Double   -> m ()) -> m () #-}
{-# SPECIALISE loop :: (Monad m) => Float    -> Float    -> (Float    -> m ()) -> m () #-}
{-# SPECIALISE loop :: (Monad m) => Int      -> Int      -> (Int      -> m ()) -> m () #-}
{-# SPECIALISE loop :: (Monad m) => Int8     -> Int8     -> (Int8     -> m ()) -> m () #-}
{-# SPECIALISE loop :: (Monad m) => Int16    -> Int16    -> (Int16    -> m ()) -> m () #-}
{-# SPECIALISE loop :: (Monad m) => Int32    -> Int32    -> (Int32    -> m ()) -> m () #-}
{-# SPECIALISE loop :: (Monad m) => Int64    -> Int64    -> (Int64    -> m ()) -> m () #-}
{-# SPECIALISE loop :: (Monad m) => Integer  -> Integer  -> (Integer  -> m ()) -> m () #-}
{-# SPECIALISE loop :: (Monad m) => Ordering -> Ordering -> (Ordering -> m ()) -> m () #-}
{-# SPECIALISE loop :: (Monad m) => Word     -> Word     -> (Word     -> m ()) -> m () #-}
{-# SPECIALISE loop :: (Monad m) => Word8    -> Word8    -> (Word8    -> m ()) -> m () #-}
{-# SPECIALISE loop :: (Monad m) => Word16   -> Word16   -> (Word16   -> m ()) -> m () #-}
{-# SPECIALISE loop :: (Monad m) => Word32   -> Word32   -> (Word32   -> m ()) -> m () #-}
{-# SPECIALISE loop :: (Monad m) => Word64   -> Word64   -> (Word64   -> m ()) -> m () #-}
