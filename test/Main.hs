{-# LANGUAGE BangPatterns #-}

import           Control.Monad.State.Strict
import           Data.IORef
import           Data.Word
import           Test.Hspec

import           Control.Loop (forLoop)


main :: IO ()
main = hspec $ do

  describe "forLoop" $ do

    it "sum [1..10], strict State" $ do
      let res = flip execState 0 $ do
            forLoop (1 :: Int) (<= 10) (+1) $ \i -> do
              x <- get
              put $! x + i

      res `shouldBe` sum [1..10]

    it "sum [-10..10], strict State" $ do
      let res = flip execState 0 $ do
            forLoop (-10 :: Int) (<= 10) (+1) $ \i -> do
              x <- get
              put $! x + i

      res `shouldBe` sum [-10..10]

    it "over all of Word32, calculating sum, IORef" $ do
      ref <- newIORef 0
      forLoop (0 :: Word32) (< maxBound) (+1) $ \i -> do
        modifyIORef' ref (+i)
      res <- readIORef ref
      res `shouldBe` 2147483649

    it "over all of Word32, calculating sum, strict State" $ do
      let res = flip execState 0 $ do
            forLoop (0 :: Word32) (< maxBound) (+1) $ \i -> do
              x <- get
              put $! x + i

      res `shouldBe` 2147483649

    it "over all of Word32, calculating sum, strict State, i unused" $ do
      let res = flip execState 0 $ do
            forLoop (0 :: Word32) (< maxBound) (+1) $ \_ -> do
              x <- get
              put $! x + 1

      res `shouldBe` (maxBound :: Word32)
