{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Capability.Reflection
  ( -- * Reflection
    Reflected (..)
  , unreflect
  , Reified
  , reified
    -- * Re-exported
  , Reifies
  , reify
  , reflect
  , Proxy (..)
  ) where

import Capability.Constraints
--import Capability.Derive
--import Capability.TagOf
import Data.Proxy
import Data.Reflection

newtype Reflected (s :: *) (c :: Capability) (m :: * -> *) (a :: *) = Reflect (m a)
  deriving (Functor, Applicative, Monad)

unreflect :: proxy s -> Reflected s c m a -> m a
unreflect _ (Reflect m) = m

data family Reified (c :: Capability) (m :: * -> *)

--class Reifiable c where
--  data Reified (c :: (* -> *) -> Constraint) (m :: * -> *)
--  reified :: Monad m => Reifies s (Def c m) :- c (Reified c m s)

reified :: forall s c m. Reifies s (Reified c m) => Reified c m
reified = reflect (Proxy @s)

--interpret
--  :: forall tag (c :: Capability) (cs :: [Capability]) m a.
--  ( TagOf c tag
--  , All cs m
--  )
--  => Reified c m
--  -> (forall m'. All (c ': cs) m' => m' a)
--  -> m a
--interpret dict action = reify dict $ \(_ :: Proxy s) ->
--  derive @(Reflected s) @'[c] @cs action
