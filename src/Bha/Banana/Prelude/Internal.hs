{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving,
             NoImplicitPrelude #-}

module Bha.Banana.Prelude.Internal
  ( Banana(..)
  ) where

import Reactive.Banana
import Reactive.Banana.Frameworks

import Bha.Prelude

-- | A wrapper around MomentIO, used to control what effects games are allowed
-- to use.
newtype Banana a
  = Banana { unBanana :: MomentIO a }
  deriving newtype (Applicative, Functor, Monad, MonadFix, MonadMoment)
