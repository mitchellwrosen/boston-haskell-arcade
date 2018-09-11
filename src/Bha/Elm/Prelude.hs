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
    -- * Re-exports
  , module X
  ) where

import Control.Applicative as X (empty)
import Control.Arrow       as X ((>>>))
import Control.Lens        as X (use, (%=), (%~), (+=), (.=), (.~), (^.))
import Control.Lens.TH     as X (makeFields)
import Control.Lens.Zoom   as X (zoom)
import Control.Monad.State as X (StateT, get, put, runState)
import Termbox.Banana      as X (Event(..), Key(..))

import Bha.Prelude              as X
import Bha.View                 as X
import Internal.Bha.Elm.Prelude
