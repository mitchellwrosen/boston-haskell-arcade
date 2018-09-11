module Bha.Banana.Prelude
  ( -- * Terminal
    TermEvent
    -- * Banana
  , Banana
  , Events
  , Behavior
  , MonadMoment
  , (<@)
  , (<@>)
  , accumB
  , accumE
  , executeE
  , filterCoincidentE
  , filterE
  , filterJust
  , leftmostE
  , mapMaybeE
  , mergeE
  , never
  , previewE
  , reactimate
  , stepper
  , switchB
  , switchE
  , unionWith
  , unions
  , unpairE
  , whenE
    -- * Randomness
  , randomBool
  , randomInt
  , randomOneOf
  , randomPct
    -- * Re-exports
  , module X
  ) where

import Control.Lens   as X (at, ix, (%~), (.~))
import Termbox.Banana as X (Cursor(..), Event(..), Key(..))

import Bha.Prelude                 as X
import Bha.View                    as X
import Internal.Bha.Banana.Prelude
