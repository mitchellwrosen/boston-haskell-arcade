module Bha.Elm.Prelude
  ( -- * Elm game
    ElmGame(..)
  , MonadElm
  , Init
  , Update
  , gameover
    -- ** Persistence
  , save
  , load
    -- ** Randomness
  , randomInt
  , randomPct
  , module X
  ) where

import Bha.Elm.Prelude.Internal
import Bha.Prelude              as X
import Bha.View                 as X

import Control.Applicative as X (empty)
import Control.Arrow       as X ((>>>))
import Control.Lens        as X (use, (%=), (%~), (+=), (.=), (.~), (^.))
import Control.Lens.TH     as X (makeFields)
import Control.Lens.Zoom   as X (zoom)
import Control.Monad.State as X (StateT, get, put, runState)
import Termbox.Banana      as X (Event(..), Key(..))
