loop
====

[![Build Status](https://travis-ci.org/nh2/loop.png)](https://travis-ci.org/nh2/loop)

Fast loops for Haskell (for when GHC can't optimize `forM_`), with benchmarks.

**Summary:**

`forM_ [0..n]` is slow. This gives you:

```haskell
import Control.Loop (forLoop)

  forLoop 0 (< n) (+1) $ \i -> do
     -- high performance loop body here
```

If you want an overflow check, use `succ` instead of `(+1)`; it's a bit slower.

On [Hackage](http://hackage.haskell.org/package/loop): `cabal install loop`


The problem
-----------

You (and everybody else) would like to write

```haskell
forM_ [0..n] $ \i -> ...
```

Clearly GHC should optimize this list away, yielding a nice loop in the generated assembly.

Unfortunately, this [isn't always true](https://ghc.haskell.org/trac/ghc/ticket/8763), and at the moment, it really doesn't happen reliably.


Current most appealing solution for fast loops in Haskell
---------------------------------------------------------

Unfortunately, this seems to be the most robustly fast (across all types I have tested it with) loop:

```haskell
forLoop :: (Monad m) => a -> (a -> Bool) -> (a -> a) -> (a -> m ()) -> m ()
forLoop start cond inc f = go start
  where
    go !x | cond x    = f x >> go (inc x)
          | otherwise = return ()

{-# INLINE loop #-}
```

And you can use it as

```haskell
forLoop 0 (< n) (+1) $ \i -> ...
```

Now, you probably don't like this, and neither do I, but it really looks like this is what you should use if you want to write high performance code.

It looks like C, but at least it's just as fast.


Does it really matter?
----------------------

One might think that in many cases the loop incrementing performance doesn't matter too much, since the loop body should dominate the time spent.

That is true, but it is easy to get into cases where it doesn't. Some examples:

* You want to write a test to make sure that [reinterpret-casting](http://stackoverflow.com/a/7002812/263061) a Word32 to Float and back gives you the same Word32. Using `forLoop` can make the difference if you have to wait 40 seconds for your test to pass or 3 seconds. (This is how I got into this topic.)

* You want to implement something that does trivial operations on unboxed vectors, say dot product or matrix multiplication. `forLoop` can make a 5x performance difference.


Shouldn't I write something this trivial ad-hoc?
------------------------------------------------

* Maintaining the fastest way to loop in one place frees one from thinking about it, and I plan to keep this updated with the fastest implementation known (contributions welcome).

* It gives us a place to discuss alternatives and collect benchmarks for high-performance looping.


Origin
------

This originates from the thread [How to write fast for loops](http://www.haskell.org/pipermail/haskell-cafe/2014-April/113902.html) on Haskell-Cafe.


Benchmarks
----------

* [bench/Bench.hs results](https://rawgit.com/nh2/loop/master/results/bench.html)
* [bench/TraverseW32.hs results](https://rawgit.com/nh2/loop/master/results/bench-traverse-w32.html)
* [bench/FoldlAndIORefAreSlow.hs results](https://rawgit.com/nh2/loop/master/results/bench-foldl-and-iorefs-are-slow.html) - see also
  [llvm](https://rawgit.com/nh2/loop/master/results/bench-foldl-and-iorefs-are-slow-llvm.html),
  [ghc7.8](https://rawgit.com/nh2/loop/master/results/bench-foldl-and-iorefs-are-slow-7.8.html),
  [ghc7.8/llvm](https://rawgit.com/nh2/loop/master/results/bench-foldl-and-iorefs-are-slow-7.8-llvm.html)

Some results:

* It is tricky to write an Enum-based general loop function that is fast for all data types.
  The way overflow checks are implemented in different types makes e.g. Int and Word32 behave differently; Word32 is slower.
* `[a..b]` is nice and can be just as fast as a manual loop - if you are lucky.
   Unfortunately, if you don't want to rely on luck for your program's performance, you can't have your `[a..b]` (at the time being).
* Vector's equivalent of `[a..b]` *might* not be as luck dependent, but suffers from a factor 5 penalty, which is hopefully a but (see John's post).
