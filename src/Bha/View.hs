{-# LANGUAGE NoImplicitPrelude #-}

module Bha.View
  ( tbstr
  ) where

import Termbox.Banana (Attr, Cell(..), Cells)

import qualified Termbox.Banana as Tb

import Bha.Prelude

tbstr :: Int -> Int -> Attr -> Attr -> [Char] -> Cells
tbstr col0 row fg bg =
  foldMap (\(col, c) -> Tb.set col row (Cell c fg bg)) . zip [col0..]
