{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Capability.Reflection where

import Data.Constraint
import Data.Constraint.Unsafe
import Data.Proxy
import Data.Reflection

-- XXX: Are @Reified@ and @reflected@ good names?
newtype Reified (c :: (* -> *) -> Constraint) m s a = Reified { reflected :: m a }
  deriving (Functor, Applicative, Monad)

class Reifiable c where
  data Def (c :: (* -> *) -> Constraint) (m :: * -> *) :: *
  -- XXX: Is this a good place for the @Monad@ constraint?
  reified :: Monad m => Reifies s (Def c m) :- c (Reified c m s)

interpret :: forall c m a. (Reifiable c, Monad m) => Def c m -> (c m => m a) -> m a
interpret d m = reify d $ \(_ :: Proxy s) ->
  let replaceProof :: Reifies s (Def c m) :- c m
      replaceProof = trans proof reified
        where proof = unsafeCoerceConstraint :: c (Reified c m s) :- c m
  in m \\ replaceProof
