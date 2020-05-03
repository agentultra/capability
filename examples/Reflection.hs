{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Reflection where

import Capability.Reflection
import Capability.Sink
import Control.Monad.State
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

----------------------------------------------------------------------
-- Interpretations

accumulate :: (forall m. HasSink "nums" Int m => m ()) -> [Int]
accumulate m = (flip execState []) $ do
  interpret
    -- XXX: Find a way to avoid the type signature
    (HasSink { _yield = \a -> modify (a:) } :: Def (HasSink "nums" Int) (State [Int]))
    m

----------------------------------------------------------------------
-- Test Cases

spec :: Spec
spec = do
  describe "accumulate" $
    it "evaluates iota" $
      accumulate (iota 10) `shouldBe` [9, 8 .. 0]
