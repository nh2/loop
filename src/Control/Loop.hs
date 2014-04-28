{-# LANGUAGE BangPatterns #-}

-- | Provides a convenient and fast alternative to the common
-- `forM_ [1..n]` idiom, which in many cases GHC cannot fuse to efficient
-- code.
module Control.Loop
  ( loop
  , unsafeLoop
  ) where


-- | @loop start end f@: Loops from @start@ to @end@ (inclusive), executing @f@
-- on each iteration. Same as @forM_ [start..end] f@.
--
-- Uses `succ` inside, which does a bounds (overflow) check.
loop :: (Enum e, Eq e, Monad m) => e -> e -> (e -> m ()) -> m ()
loop start end f = go start
  where
    go !x | x == end  = f x
          | otherwise = f x >> go (succ x)

{-# INLINE loop #-}


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

{-# INLINE unsafeLoop #-}
