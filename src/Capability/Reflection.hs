{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
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
    Reifiable (..)
  , interpret
    -- ** Implementation
  , Reified (..)
  , reflectDef
    -- * Re-exported
  , Reifies
  , Dict (..)
  , (:-) (..)
  ) where

import Capability.TagOf
import Data.Constraint
import Data.Constraint.Unsafe
import Data.Proxy
import Data.Reflection

-- XXX: Are @Reified@ and @reflected@ good names?
newtype Reified (c :: (* -> *) -> Constraint) m s a = Reified { reflected :: m a }
  deriving (Functor, Applicative, Monad)

reflectDef :: forall s c m. Reifies s (Def c m) => Def c m
reflectDef = reflect (Proxy @s)

class Reifiable c where
  data Def (c :: (* -> *) -> Constraint) (m :: * -> *) :: *
  -- XXX: Is this a good place for the @Monad@ constraint?
  reified :: Monad m => Reifies s (Def c m) :- c (Reified c m s)

interpret :: forall tag c m a. (TagOf c tag, Reifiable c, Monad m) => Def c m -> (c m => m a) -> m a
interpret d m = reify d $ \(_ :: Proxy s) ->
  let replaceProof :: Reifies s (Def c m) :- c m
      replaceProof = trans proof reified
        where proof = unsafeCoerceConstraint :: c (Reified c m s) :- c m
  in m \\ replaceProof
