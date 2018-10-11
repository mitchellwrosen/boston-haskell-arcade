{-# OPTIONS_GHC -fno-warn-orphans #-}

module Internal.Bha.Orphans () where

import Data.Aeson      (FromJSON(..), ToJSON(..))
import Data.Void       (Void, absurd)
import Prelude
import Reactive.Banana

instance Monoid a => Monoid (Behavior a) where
  mempty = pure mempty
  mappend = (<>)

instance Semigroup a => Semigroup (Behavior a) where
  (<>) = liftA2 (<>)
