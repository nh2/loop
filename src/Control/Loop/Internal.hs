--{-# LANGUAGE BangPatterns #-}

-- | This is for trying out loop alternatives.
--
-- Names and types are subjects to change.
module Control.Loop.Internal
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
    go x | x `seq` x == end = f x
         | otherwise        = f x >> go (succ x)

{-# INLINE loop #-}


-- | Like `loop`, but (sometimes) without bounds (overflow) check.
--
-- This circumvents the implementation of `succ` for the @Enum@ type
-- and uses @toEnum . (+ 1) . fromEnum@ instead, so it will break
-- on Enums that are not contiguous.
--
-- Note that some types (e.g. Word32) have bounds checks even for
-- `toEnum`.
unsafeLoop :: (Enum e, Eq e, Monad m) => e -> e -> (e -> m ()) -> m ()
unsafeLoop start end f = go start
  where
    go x | x `seq` x == end = f x
         | otherwise        = f x >> go (unsafeSucc x)
    unsafeSucc = toEnum . (+ 1) . fromEnum

{-# INLINE unsafeLoop #-}
