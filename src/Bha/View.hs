{-# LANGUAGE NoImplicitPrelude #-}

module Bha.View
  ( tbstr
  , rect
  ) where

import Termbox.Banana (Attr, Cell(..), Cells)

import qualified Termbox.Banana as Tb

import Bha.Prelude

tbstr :: Int -> Int -> Attr -> Attr -> [Char] -> Cells
tbstr col0 row fg bg =
  foldMap (\(col, c) -> Tb.set col row (Cell c fg bg)) . zip [col0..]

rect :: Int -> Int -> Int -> Int -> Attr -> Attr -> Cells
rect c0 r0 w h fg bg =
  foldMap
    (\(r, c) -> Tb.set c r (Cell ' ' fg bg))
    ((,)
      <$> [r0 .. r0 + h - 1]
      <*> [c0 .. c0 + w - 1])

