{-# LANGUAGE NoImplicitPrelude, RankNTypes #-}

module Reactive.Banana.Bha
  ( previewE
  , unpairE
  , module Reactive.Banana
  ) where

import Reactive.Banana

import Bha.Prelude

-- | Filter an 'Event' with a 'Prism''.
previewE :: Prism' s a -> Event s -> Event a
previewE p e =
  filterJust (preview p <$> e)

-- | Split an event of @(a, b)@ into two coindicent events of @a@ and @b@.
unpairE :: Event (a, b) -> (Event a, Event b)
unpairE e =
  (fst <$> e, snd <$> e)
