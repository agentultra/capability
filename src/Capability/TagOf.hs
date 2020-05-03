{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Capability.TagOf where

import Data.Constraint

-- | Extracts the @tag@ from a capability.
class TagOf (c :: (* -> *) -> Constraint) (tag :: k) | c -> tag
-- XXX: Using a type family caused issues with inference when using @interpret@.
-- XXX: Maybe we should have defined capabilites as @k -> (* -> *) -> Constraint@?
