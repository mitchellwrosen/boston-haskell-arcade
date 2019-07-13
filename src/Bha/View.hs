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

import Bha.Internal.View

import Termbox.Banana as X (Attr, Cell(..), Cursor(..))


red, green, yellow, blue, magenta, cyan, white, tangerine, orange, cement, black :: Attr
red       =   1
green     =   2
yellow    =   3
blue      =   4
magenta   =   5
cyan      =   6
white     =  15
tangerine =  16
orange    =  17
cement    =  20
black     = 232
