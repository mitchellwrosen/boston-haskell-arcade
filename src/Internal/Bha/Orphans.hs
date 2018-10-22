{-# OPTIONS_GHC -fno-warn-orphans #-}

module Internal.Bha.Orphans () where

import Mitchell.Prelude
import Data.Monoid (mappend)
import FRP (Behavior)

instance Monoid a => Monoid (Behavior a) where
  mempty = pure mempty
  mappend = (<>)

instance Semigroup a => Semigroup (Behavior a) where
  (<>) = liftA2 (<>)
