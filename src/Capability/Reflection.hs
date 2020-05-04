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
import Data.Proxy
import Data.Reflection

newtype Reflected s m a = Reflect (m a)
  deriving (Functor, Applicative, Monad)

unreflect :: proxy s -> Reflected s m a -> m a
unreflect _ (Reflect m) = m

data family Reified (c :: (* -> *) -> Constraint) (m :: * -> *)

reified :: forall s c m. Reifies s (Reified c m) => Reified c m
reified = reflect (Proxy @s)
