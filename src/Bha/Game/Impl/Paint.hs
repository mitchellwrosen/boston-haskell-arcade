module Bha.Game.Impl.Paint
  ( moment
  ) where

import Data.Maybe (fromJust)

import qualified Data.HashMap.Strict as HashMap

import Bha.Banana.Prelude

-- TODO center easel on screen

type Col = Int
type Row = Int

ccol    =  8 :: Int -- canvas top-left col
crow    =  4 :: Int -- canvas top-left row
cwidth  = 42 :: Int -- canvas width
cheight = 20 :: Int -- canvas height

data Color
  = White
  | Blue
  | Red
  | Green
  | Magenta
  | Yellow
  | Cyan
  deriving (Bounded, Enum, Eq)

moment
  :: Events TermEvent
  -> Banana (Behavior Scene, Events ())
moment eEvent = mdo
  let
    eUp    = filterE (== EventKey KeyArrowUp    False) eEvent
    eDown  = filterE (== EventKey KeyArrowDown  False) eEvent
    eLeft  = filterE (== EventKey KeyArrowLeft  False) eEvent
    eRight = filterE (== EventKey KeyArrowRight False) eEvent
    eEsc   = filterE (== EventKey KeyEsc        False) eEvent
    eEnter = filterE (== EventKey KeyEnter      False) eEvent
    eTab   = filterE (== EventKey KeyTab        False) eEvent
    eH     = filterE (== EventKey (KeyChar 'h') False) eEvent
    eJ     = filterE (== EventKey (KeyChar 'j') False) eEvent
    eK     = filterE (== EventKey (KeyChar 'k') False) eEvent
    eL     = filterE (== EventKey (KeyChar 'l') False) eEvent

  bCanvas :: Behavior (HashMap (Col, Row) Color) <-
    accumB mempty $ unions
      [ HashMap.insert <$> bCursor <*> bColor <@ eEnter
      ]

  bColor :: Behavior Color <-
    accumB White (cycleSelected <$ eTab)

  bCursor :: Behavior (Col, Row) <- do
    accumB (ccol, crow) $ unions
      [ over _2 (max crow . subtract 1) <$
          unionWith const eUp eK
      , over _2 (min (crow+cheight-1) . (+1)) <$
          unionWith const eDown eJ
      , over _1 (max ccol . subtract 1) <$
          unionWith const eLeft eH
      , over _1 (min (ccol+cwidth-1) . (+1)) <$
          unionWith const eRight eL
      ]

  let
    bCells :: Behavior Cells
    bCells =
      render <$> bCanvas <*> bCursor <*> bColor

  let
    bScene :: Behavior Scene
    bScene =
      Scene
        <$> bCells
        <*> pure NoCursor

  pure (bScene, () <$ eEsc)

cycleSelected :: Color -> Color
cycleSelected =
  fromJust . flip lookup (zip xs (tail xs))
 where
  xs = cycle [minBound..maxBound] :: [Color]

render :: HashMap (Col, Row) Color -> (Col, Row) -> Color -> Cells
render canvas cursor selected =
  mconcat
    [ renderCanvasBorder
    , renderCanvas canvas
    , renderCursor cursor selected
    , renderEasel
    , renderSelected selected
    ]

renderCanvasBorder :: Cells
renderCanvasBorder =
  mconcat
    [ rect (ccol-4)        (crow-2)         (cwidth+8) 1           white
    , rect (ccol-4)        (crow+cheight+1) (cwidth+8) 1           red
    , rect (ccol-4)        (crow-1)         2          (cheight+2) blue
    , rect (ccol+cwidth+2) (crow-1)         2          (cheight+2) yellow
    ]

renderCanvas :: HashMap (Col, Row) Color -> Cells
renderCanvas =
  foldMap (\((c, r), x) -> set c r (Cell ' ' mempty (attr x))) . HashMap.toList

renderCursor :: (Col, Row) -> Color -> Cells
renderCursor (c, r) color =
  set c r (Cell ' ' mempty (attr color))

renderEasel :: Cells
renderEasel =
  foldMap
    (\(i, c) ->
      let
        w = 6 :: Int
        h = 3 :: Int
      in
        rect (ccol+i*w) (crow+cheight+h) w h (attr c))
    (zip [0..] [minBound..maxBound])

renderSelected :: Color -> Cells
renderSelected color =
  text
    (ccol + fromEnum color * 6 + 2)
    (crow+cheight+6)
    mempty
    mempty
    "••"

attr :: Color -> Attr
attr = \case
  White   -> white
  Blue    -> blue
  Red     -> red
  Green   -> green
  Magenta -> magenta
  Yellow  -> yellow
  Cyan    -> cyan
