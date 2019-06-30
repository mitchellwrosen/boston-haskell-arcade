{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Unfortunate orphan instances.

module Internal.Bha.Orphans () where

import Control.Applicative
import Data.Monoid         (Monoid(..))
import Data.Semigroup      (Semigroup(..))
import Reactive.Banana     (Behavior)

instance Monoid a => Monoid (Behavior a) where
  mempty = pure mempty
  mappend = (<>)

instance Semigroup a => Semigroup (Behavior a) where
  (<>) = liftA2 (<>)
