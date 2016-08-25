--{-# LANGUAGE BangPatterns #-}

-- | Provides a convenient and fast alternative to the common
-- @forM_ [1..n]@ idiom, which in many cases GHC cannot fuse to efficient
-- code.
--
-- Notes on fast iteration:
--
-- * For `Int`, @(+1)@ is almost twice as fast as `succ` because `succ`
--   does an overflow check.
--
-- * For `Int`, you can get around that while still using `Enum` using
--   @toEnum . (+ 1) . fromEnum@.
--
-- * However, @toEnum . (+ 1) . fromEnum@ is slower than `succ` for
--   `Word32` on 64-bit machines since `toEnum` has to check if the
--   given `Int` exceeds 32 bits.
--
-- * Using @(+1)@ from `Num` is always the fastest way, but it gives
--   no overflow checking.
--
-- * Using `forLoop` you can flexibly pick the way of increasing the value
--   that best fits your needs.
--
-- * The currently recommended replacement for @forM_ [1..n]@ is
--   @forLoop 1 (<= n) (+1)@.
module Control.Loop
  ( forLoop
  , forLoopState
  , forLoopFold
  , numLoop
  , numLoopState
  , numLoopFold
  ) where


-- | @forLoop start cond inc f@: A C-style for loop with starting value,
-- loop condition and incrementor.
forLoop :: (Monad m) => a -> (a -> Bool) -> (a -> a) -> (a -> m ()) -> m ()
forLoop start cond inc f = go start
  where
    go x | x `seq` False = undefined
         | cond x    = f x >> go (inc x)
         | otherwise = return ()

{-# INLINE forLoop #-}


-- | @forLoopState start cond inc initialState f@: A C-style for loop with
-- starting value, loop condition, incrementor and a state that is threaded
-- through the computation.
forLoopState :: (Monad m) => a -> (a -> Bool) -> (a -> a) -> b -> (b -> a -> m b) -> m b
forLoopState start cond inc initialState f = go start initialState
  where
    go x state | x `seq` state `seq` False = undefined
               | cond x    = f state x >>= go (inc x)
               | otherwise = return state

{-# INLINE forLoopState #-}

-- | @forLoopFold start cond inc acc0 f@: A pure fold using a for loop
-- instead of a list for performance.
--
-- Care is taken that @acc0@ not be strictly evaluated if unless done so by @f@.
forLoopFold :: a -> (a -> Bool) -> (a -> a) -> acc -> (acc -> a -> acc) -> acc
forLoopFold start cond inc acc0 f = go acc0 start
  where
    -- Not using !acc, see:
    --   http://neilmitchell.blogspot.co.uk/2013/08/destroying-performance-with-strictness.html
    go acc x | x `seq` False = undefined
             | cond x    = let acc' = f acc x
                           in acc' `seq` go acc' (inc x)
             | otherwise = acc

{-# INLINE forLoopFold #-}


-- | @numLoop start end f@: Loops over a contiguous numerical range, including
-- @end@.
--
-- Does nothing when not @start <= end@.
--
-- It uses @(+ 1)@ so for most integer types it has no bounds (overflow) check.
numLoop :: (Num a, Ord a, Monad m) => a -> a -> (a -> m ()) -> m ()
numLoop start end f = if start <= end then go start else return ()
  where
    go x | x `seq` False = undefined
         | x == end  = f x
         | otherwise = f x >> go (x+1)

{-# INLINE numLoop #-}

-- | @numLoopState start end f initialState@: Loops over a contiguous numerical
-- range, including @end@ threading a state through the computation.
--
-- It uses @(+ 1)@ so for most integer types it has no bounds (overflow) check.
numLoopState :: (Num a, Eq a, Monad m) => a -> a -> b -> (b -> a -> m b) -> m b
numLoopState start end initState f = go start initState
  where
    go x state | x `seq` state `seq` False = undefined
               | x == end  = f state x
               | otherwise = f state x >>= go (x+1)


{-# INLINE numLoopState #-}

-- | @numLoopFold start end acc0 f@: A pure fold over a contiguous numerical
-- range, including @end@.
--
-- It uses @(+ 1)@ so for most integer types it has no bounds (overflow) check.
--
-- Care is taken that @acc0@ not be strictly evaluated if unless done so by @f@.
numLoopFold :: (Num a, Eq a) => a -> a -> acc -> (acc -> a -> acc) -> acc
numLoopFold start end acc0 f = end `seq` go acc0 start
  where
    go acc x | x `seq` False = undefined
             | x == end  = f acc x
             | otherwise = let acc' = f acc x
                           in acc' `seq` go acc' (x+1)

{-# INLINE numLoopFold #-}
