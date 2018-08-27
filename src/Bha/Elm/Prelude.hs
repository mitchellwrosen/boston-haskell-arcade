{-# LANGUAGE NoImplicitPrelude #-}

module Bha.Elm.Prelude
  ( -- * Elm game
    ElmGame(..)
    -- * Randomness
  , Seed
  , randomInt
  , randomIntS
    -- * Rendering
  , Scene(..)
  , Cells
  , set
  , Cell(..)
  , Cursor(..)
  , Event(..)
  , Key(..)
  , black
  , blue
  , cyan
  , green
  , magenta
  , red
  , white
  , yellow
  , module X
  ) where

import Bha.Elm.Prelude.Internal (Seed(..))
import Bha.Prelude              as X

import           Control.Applicative as X (empty)
import           Control.Arrow       as X ((>>>))
import           Control.Lens        as X (use, (%=), (%~), (.=), (.~), (^.))
import           Control.Lens.TH     as X (makeFields)
import           Control.Lens.Zoom   as X (zoom)
import           Control.Monad.State as X (StateT, get, put)
import           Control.Monad.State
import           Data.Coerce
import qualified System.Random       as Random
import           Termbox.Banana      (Cell(..), Cells, Cursor(..), Event(..),
                                      Key(..), Scene(..), black, blue, cyan,
                                      green, magenta, red, set, white, yellow)

-- | An Elm-style game.
data ElmGame model
  = ElmGame
      (Seed -> model)
      -- Initial model, given a random seed.
      (Either NominalDiffTime Event -> StateT model Maybe ())
      -- Update the model from a tick or terminal event. Return Nothing if the
      -- game is done.
      (model -> Scene)
      -- Render the model.
      (model -> Maybe NominalDiffTime)
      -- Tick, and if so, how often?

-- | Generate a random 'Int' in the given bounds (inclusive).
randomInt :: Int -> Int -> Seed -> (Int, Seed)
randomInt lo hi (Seed seed) =
  coerce (Random.randomR (lo, hi) seed)

-- | Like 'randomInt', but in the 'State' monad.
randomIntS :: Monad m => Int -> Int -> StateT Seed m Int
randomIntS lo hi =
  state (randomInt lo hi)
