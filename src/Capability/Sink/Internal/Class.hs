{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK hide #-}

module Capability.Sink.Internal.Class where

import Capability.Reflection
import Capability.TagOf
import Data.Coerce (coerce)
import GHC.Exts (Proxy#, proxy#)

-- | Sinking capability.
--
-- An instance does not need to fulfill any additional laws
-- besides the monad laws.
class Monad m
  => HasSink (tag :: k) (a :: *) (m :: * -> *) | tag m -> a
  where
    -- | For technical reasons, this method needs an extra proxy argument.
    -- You only need it if you are defining new instances of 'HasSink'.
    -- Otherwise, you will want to use 'yield'.
    -- See 'yield' for more documentation.
    yield_ :: Proxy# tag -> a -> m ()

instance TagOf (HasSink (tag :: k) a) tag

-- | @yield \@tag a@
-- emits @a@ in the sink capability @tag@.
yield :: forall tag a m. HasSink tag a m => a -> m ()
yield = yield_ (proxy# @_ @tag)
{-# INLINE yield #-}

--------------------------------------------------------------------------------

-- XXX: Should this go into its own module?
instance Reifiable (HasSink tag a) where
  data Def (HasSink tag a) m = HasSink
    { _yield :: a -> m ()
    }
  reified = Sub Dict

instance
  ( Monad m
  , Reifies s (Def (HasSink tag a) m) )
  => HasSink tag a (Reified (HasSink tag a) m s)
  where
    yield_ _ = coerce $ _yield (reflectDef @s)
