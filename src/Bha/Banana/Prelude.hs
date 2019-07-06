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
    -- * Event predicates
  , isKeyArrowDown
  , isKeyArrowLeft
  , isKeyArrowRight
  , isKeyArrowUp
  , isKeyEsc
  , isKeyChar
    -- * Re-exports
  , module X
  ) where

import Bha.Prelude                 as X
import Bha.View                    as X
import Internal.Bha.Banana.Prelude

import Termbox.Banana as X (Cursor(..), Event(..), Key(..))


isKeyArrowDown :: Event -> Bool
isKeyArrowDown = \case
  EventKey KeyArrowDown _ -> True
  _ -> False

isKeyArrowLeft :: Event -> Bool
isKeyArrowLeft = \case
  EventKey KeyArrowLeft _ -> True
  _ -> False

isKeyArrowRight :: Event -> Bool
isKeyArrowRight = \case
  EventKey KeyArrowRight _ -> True
  _ -> False

isKeyArrowUp :: Event -> Bool
isKeyArrowUp = \case
  EventKey KeyArrowUp _ -> True
  _ -> False

isKeyEsc :: Event -> Bool
isKeyEsc = \case
  EventKey KeyEsc _ -> True
  _ -> False

isKeyChar :: Char -> Event -> Bool
isKeyChar x = \case
  EventKey (KeyChar y) _ -> x == y
  _ -> False
