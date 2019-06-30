module Bha.Elm.Prelude
  ( -- * Elm game
    ElmGame(..)
  , Input(..)
  , MonadElm
  , Init
  , Update
  , gameover
    -- ** Randomness
  , randomInt
  , randomPct
    -- **
  , send
    -- * Re-exports
  , module X
  ) where

import Control.Lens        as X (use, zoom, (%=), (.=))
import Control.Lens.TH     as X (makeFields)
import Control.Monad.State as X (StateT, get, put, runState)
import Termbox.Banana      as X (Event(..), Key(..))

import Bha.Prelude              as X
import Bha.View                 as X
import Internal.Bha.Elm.Prelude
