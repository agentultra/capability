{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Reflection where

import Capability.Reflection
import Capability.Sink
import Capability.Writer
import Control.Monad.State
import Data.Monoid (Sum (..))
import Test.Hspec

----------------------------------------------------------------------
-- Example Programs

iota :: HasSink "nums" Int m => Int -> m ()
iota n
  | n < 0 = error "negative number passed to iota."
  | otherwise = go 0
  where
    go i
      | i == n = pure ()
      | otherwise = yield @"nums" i >> go (succ i)

useWriter :: HasWriter "count-writer" (Sum Int) m => m ()
useWriter = do
  -- Add 3 and retrieve result
  ((), count) <- listen @"count-writer" (tell @"count-writer" 3)
  -- Duplicate
  tell @"count-writer" count

----------------------------------------------------------------------
-- Interpretations

accumulate :: (forall m. HasSink "nums" Int m => m ()) -> [Int]
accumulate m = (flip execState []) $
  interpret @"nums"
    HasSink {_yield = \a -> modify (a :)}
    m

sumWriter :: (forall m. HasWriter "count-writer" (Sum Int) m => m ()) -> Int
sumWriter m = getSum $ (flip execState (Sum 0)) $ do
  interpret @"count-writer"
    HasWriter
      { _writerSink = HasSink { _yield = \a -> modify (a<>) }
      , _writer = undefined
      , _listen = \m' -> do
          w0 <- get
          put mempty
          a <- m'
          w <- get
          put (w0 <> w)
          pure (a, w)
      , _pass = undefined
      }
      m

----------------------------------------------------------------------
-- Test Cases

spec :: Spec
spec = do
  describe "accumulate" $
    it "evaluates iota" $
      accumulate (iota 10) `shouldBe` [9, 8 .. 0]
  describe "sumWriter" $
    it "evaluates useWriter" $
      sumWriter useWriter `shouldBe` 6
