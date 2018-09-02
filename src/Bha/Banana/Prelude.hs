{-# LANGUAGE NoImplicitPrelude, ScopedTypeVariables #-}

module Bha.Banana.Prelude
  ( TermEvent
  , executeE
    -- * Randomness
  , randomInt
  , randomOneOf
  , module X
  ) where

import Control.Lens               as X (at, ix, (%~), (.~))
import Reactive.Banana.Bha        as X
import Reactive.Banana.Frameworks (execute)
import System.Random
import Termbox.Banana             as X (Cells, Cursor(..), Event(..), Key(..),
                                        Scene(..))

import Bha.Banana.Prelude.Internal
import Bha.Banana.Prelude.Internal as X (Banana)
import Bha.Game                    as X (GameOutput(..))
import Bha.Prelude                 as X
import Bha.View                    as X

type TermEvent
  = Event

executeE :: Events (Banana a) -> Banana (Events a)
executeE e =
  Banana (execute (unBanana <$> e))

randomInt :: Int -> Int -> Banana Int
randomInt x y =
  Banana (liftIO (randomRIO (x, y)))

randomOneOf :: [a] -> Banana a
randomOneOf [] = error "randomOneOf: []"
randomOneOf xs =
  (xs !!) <$> randomInt 0 (length xs - 1)
