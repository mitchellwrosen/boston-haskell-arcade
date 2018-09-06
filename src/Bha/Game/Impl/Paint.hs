module Bha.Game.Impl.Paint
  ( moment
  ) where

import Bha.Banana.Prelude

import qualified Data.HashMap.Strict as HashMap

type Col = Int
type Row = Int

ccol    =  5 :: Int -- canvas top-left col
crow    =  5 :: Int -- canvas top-left row
cwidth  = 40 :: Int -- canvas width
cheight = 20 :: Int -- canvas height

data Color
  = White

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

  bCanvas :: Behavior (HashMap (Col, Row) Color) <-
    accumB mempty $ unions
      [ filterJust
          ((\case
            Cursor c r ->
              Just (HashMap.insert (c, r) White)
            NoCursor ->
              Nothing)
          <$> bCursor <@ eEnter)
      ]

  let
    bCells :: Behavior Cells
    bCells =
      render <$> bCanvas

  bCursor :: Behavior Cursor <-
    accumB (Cursor ccol crow) $ unions
      [ over cursorRowL (max crow             . subtract 1) <$ eUp
      , over cursorRowL (min (crow+cheight-1) . (+1))       <$ eDown
      , over cursorColL (max ccol             . subtract 1) <$ eLeft
      , over cursorColL (min (ccol+cwidth-1)  . (+1))       <$ eRight
      ]

  let
    bScene :: Behavior Scene
    bScene =
      Scene
        <$> bCells
        <*> bCursor

  pure (bScene, () <$ eEsc)

render :: HashMap (Col, Row) Color -> Cells
render =
  foldMap (\((c, r), x) -> set c r (Cell ' ' mempty (attr x))) . HashMap.toList

attr :: Color -> Attr
attr = \case
  White -> white
