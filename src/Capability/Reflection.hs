{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Capability.Reflection
  ( -- * Reflection
    interpret
  , Reflected (..)
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
import Capability.Derive
import Capability.TagOf
import Data.Proxy
import Data.Reflection

newtype Reflected (s :: *) (c :: Capability) (m :: * -> *) (a :: *) = Reflect (m a)
  deriving (Functor, Applicative, Monad)

unreflect :: proxy s -> Reflected s c m a -> m a
unreflect _ (Reflect m) = m

data family Reified (c :: Capability) (m :: * -> *)

reified :: forall s c m. Reifies s (Reified c m) => Reified c m
reified = reflect (Proxy @s)

interpret ::
  forall tag (cs :: [Capability]) c m a.
  ( TagOf c tag,
    forall s m'. (Monad m', Reifies s (Reified c m')) => c (Reflected s c m'),
    Monad m,
    All cs m
  ) =>
  Reified c m ->
  (forall m'. All (c ': cs) m' => m' a) ->
  m a
interpret dict action =
  reify dict $ \(_ :: Proxy s) ->
    derive @(Reflected s c) @'[c] @cs action
