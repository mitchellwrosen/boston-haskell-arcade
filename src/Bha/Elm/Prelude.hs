{-# LANGUAGE ExistentialQuantification, NoImplicitPrelude #-}

module Bha.Elm.Prelude
  ( ElmGame(..)
  , module X
  ) where

import Bha.Prelude as X

-- TODO Export nicer randomness API
import System.Random  as X (StdGen, random)
import Termbox.Banana as X (Cell(..), Cells, Cursor(..), Event(..), Key(..),
                            Scene(..), set)

-- | An Elm-style game.
data ElmGame
  = forall model. ElmGame
      (StdGen -> model)
      -- Initial model, given a random seed.
      (Either NominalDiffTime Event -> model -> Maybe model)
      -- Update the model from a tick or terminal event. Return Nothing if the
      -- game is done.
      (model -> Scene)
      -- Render the model.
      (model -> Maybe NominalDiffTime)
      -- Tick, and if so, how often?
