-- | Game types. A game is written in either Elm-style or FRP-style.

{-# LANGUAGE ExistentialQuantification, NoImplicitPrelude #-}

module Bha.Game
  ( Game(..)
  , ElmGame(..)
  ) where

import Bha.Prelude

import qualified Termbox.Banana as Tb

data Game
  = GameElm ElmGame -- ^ An Elm-style game.
  | GameFRP         -- ^ An FRP-style game. TODO Implement this

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
