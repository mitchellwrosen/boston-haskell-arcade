module Bha.View
  ( cursorColL
  , cursorRowL
  , text
  , rect
  , module X
  ) where

import Termbox.Banana as X (Attr, Cell(..), Cells, Cursor(..), Scene(..), black,
                            blue, cyan, green, magenta, red, set, white, yellow)

import qualified Termbox.Banana as Tb

import Bha.Prelude

cursorColL :: Traversal' Cursor Int
cursorColL f = \case
  NoCursor -> pure NoCursor
  Cursor c r -> Cursor <$> f c <*> pure r

cursorRowL :: Traversal' Cursor Int
cursorRowL f = \case
  NoCursor -> pure NoCursor
  Cursor c r -> Cursor <$> pure c <*> f r

text :: Int -> Int -> Attr -> Attr -> String -> Cells
text col0 row fg bg =
  foldMap (\(col, c) -> Tb.set col row (Cell c fg bg)) . zip [col0..]

rect :: Int -> Int -> Int -> Int -> Attr -> Cells
rect c0 r0 w h bg =
  foldMap
    (\(r, c) -> Tb.set c r (Cell ' ' mempty bg))
    ((,)
      <$> [r0 .. r0 + h - 1]
      <*> [c0 .. c0 + w - 1])
