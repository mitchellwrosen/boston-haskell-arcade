-- | Game types. A game is written in either Elm-style or FRP-style.

{-# LANGUAGE DerivingStrategies, ExistentialQuantification,
             GeneralizedNewtypeDeriving, NoImplicitPrelude #-}

module Bha.Game
  ( Game(..)
  , ElmGame(..)
  , BananaGame
  , Banana(..)
  ) where

import Reactive.Banana
import Reactive.Banana.Frameworks (MomentIO)
import Termbox.Banana             (Scene)

import Bha.Prelude

import qualified Termbox.Banana as Tb

data Game
  = GameElm ElmGame       -- ^ An Elm-style game.
  | GameBanana BananaGame -- ^ An FRP-style game.

-- | An Elm-style game.
data ElmGame = forall a. ElmGame
  { init :: a
    -- ^ Initial model.

  , update :: Either NominalDiffTime Tb.Event -> a -> a
    -- ^ Update the model from a tick or terminal event.

  , view :: a -> Tb.Scene
    -- ^ Render the model.

  , isDone :: a -> Bool
    -- ^ Is the game done?

  , tickEvery :: a -> Maybe NominalDiffTime
    -- ^ Tick, and if so, how often?
  }

-- | An FRP-style game.
type BananaGame
  = Event Tb.Event -> Banana (Behavior Scene, Event ())

-- | A wrapper around MomentIO, used to control what effects games are allowed
-- to use.
newtype Banana a
  = Banana { unBanana :: MomentIO a }
  deriving newtype (Applicative, Functor, Monad, MonadFix, MonadMoment)
