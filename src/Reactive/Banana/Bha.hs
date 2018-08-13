{-# LANGUAGE NoImplicitPrelude, RankNTypes #-}

module Reactive.Banana.Bha
  ( Events
  , previewE
  , unpairE
  , module Reactive.Banana
  ) where

import Reactive.Banana hiding (Event)

import qualified Reactive.Banana

import Bha.Prelude

type Events
  = Reactive.Banana.Event

-- | Filter an 'Event' with a 'Prism''.
previewE :: Prism' s a -> Events s -> Events a
previewE p e =
  filterJust (preview p <$> e)

-- | Split an event of @(a, b)@ into two coindicent events of @a@ and @b@.
unpairE :: Events (a, b) -> (Events a, Events b)
unpairE e =
  (fst <$> e, snd <$> e)
