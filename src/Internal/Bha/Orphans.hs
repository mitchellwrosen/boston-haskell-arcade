{-# OPTIONS_GHC -fno-warn-orphans #-}

module Internal.Bha.Orphans () where

import Reactive.Banana
import Prelude

instance Monoid a => Monoid (Behavior a) where
  mempty = pure mempty
  mappend = (<>)

instance Semigroup a => Semigroup (Behavior a) where
  (<>) = liftA2 (<>)
