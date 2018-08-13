{-# LANGUAGE NoImplicitPrelude #-}

module Bha.Banana.Prelude
  ( TermEvent
  , module X
  ) where

import Reactive.Banana.Bha as X
import Termbox.Banana      as X (Cells, Cursor(..), Event(..), Key(..),
                                 Scene(..))

import Bha.Banana.Prelude.Internal as X (Banana)
import Bha.Prelude                 as X
import Bha.View                    as X

type TermEvent
  = Event
