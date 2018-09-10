{-# OPTIONS_GHC -fno-warn-missing-exported-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Bha.View
  ( Cells
  , set
  , Scene(..)
  , cursorColL
  , cursorRowL
  , text
  , rect
  , rect'
    -- ** Named colors
  , black
  , blue
  , cement
  , cyan
  , green
  , magenta
  , orange
  , tangerine
  , red
  , white
  , yellow
  , module X
  ) where

import Termbox.Banana as X (Attr, Cell(..), Cursor(..))

import Internal.Bha.View

red       =   1 :: Attr
green     =   2 :: Attr
yellow    =   3 :: Attr
blue      =   4 :: Attr
magenta   =   5 :: Attr
cyan      =   6 :: Attr
white     =  15 :: Attr
tangerine =  16 :: Attr
orange    =  17 :: Attr
cement    =  20 :: Attr
black     = 232 :: Attr
