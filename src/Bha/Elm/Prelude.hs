{-# LANGUAGE ExistentialQuantification, NoImplicitPrelude #-}

module Bha.Elm.Prelude
  ( ElmGame(..)
  , module X
  ) where

import Bha.Prelude as X

import Termbox.Banana (Event, Scene)

-- | An Elm-style game.
data ElmGame
  = forall model. ElmGame
      model
      -- Initial model.
      (Either NominalDiffTime Event -> model -> model)
      -- Update the model from a tick or terminal event.
      (model -> Scene)
      -- Render the model.
      (model -> Bool)
      -- Is the game done?
      (model -> Maybe NominalDiffTime)
      -- Tick, and if so, how often?
