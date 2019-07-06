module Bha.Game.Impl.Paint
  ( moment
  ) where

import Bha.Banana.Prelude

import Data.Maybe (fromJust)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List           as List


-- TODO center easel on screen

type Col = Int
type Row = Int

ccol    =  8 :: Int -- canvas top-left col
crow    =  4 :: Int -- canvas top-left row
cwidth  = 50 :: Int -- canvas width
cheight = 20 :: Int -- canvas height

data Color
  = White
  | Blue
  | Red
  | Green
  | Magenta
  | Yellow
  | Cyan
  | Black
  deriving (Bounded, Enum, Eq)

moment
  :: Int
  -> Int
  -> Events (Text, Void)
  -> Events Key
  -> Banana
       ( Behavior Scene
       , Behavior (HashSet Text)
       , Events (Text, Void)
       , Events ()
       )
moment _ _ _ eKey = mdo
  let
    eUp        = filterE (== KeyArrowUp) eKey
    eDown      = filterE (== KeyArrowDown) eKey
    eLeft      = filterE (== KeyArrowLeft) eKey
    eRight     = filterE (== KeyArrowRight) eKey
    eEsc       = filterE (== KeyEsc) eKey
    eEnter     = filterE (== KeyEnter) eKey
    eSpace     = filterE (== KeySpace) eKey
    eBackspace = filterE (== KeyBackspace2) eKey
    eTab       = filterE (== KeyTab) eKey
    eH         = filterE (== KeyChar 'h') eKey
    eJ         = filterE (== KeyChar 'j') eKey
    eK         = filterE (== KeyChar 'k') eKey
    eL         = filterE (== KeyChar 'l') eKey

  bBackground :: Behavior Bool <-
    accumB True (not <$ eSpace)

  bCanvas :: Behavior (HashMap (Col, Row) Color) <-
    accumB mempty $ unions
      [ HashMap.insert <$> bCursor <*> bColor <@ eEnter
      , HashMap.delete <$> bCursor <@ eBackspace
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
      render <$> bBackground <*> bCanvas <*> bCursor <*> bColor

  let
    bScene :: Behavior Scene
    bScene =
      Scene
        <$> bCells
        <*> pure NoCursor

  pure (bScene, pure mempty, never, () <$ eEsc)

cycleSelected :: Color -> Color
cycleSelected =
  fromJust . flip List.lookup (List.zip xs (List.tail xs))
 where
  xs = cycle [minBound..maxBound] :: [Color]

render :: Bool -> HashMap (Col, Row) Color -> (Col, Row) -> Color -> Cells
render background canvas cursor selected =
  mconcat
    [ renderCanvasBorder
    , renderCanvas background canvas
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

renderCanvas :: Bool -> HashMap (Col, Row) Color -> Cells
renderCanvas background cells =
  mconcat
    [ if background
        then
          foldMap
            (\(c, r) -> set c r (Cell '‧' mempty mempty))
            ((,) <$> [ccol..ccol+cwidth-1] <*> [crow..crow+cheight-1])
        else
          mempty

    , foldMap
        (\((c, r), x) -> set c r (Cell ' ' mempty (attr x)))
        (HashMap.toList cells)
    ]

renderCursor :: (Col, Row) -> Color -> Cells
renderCursor (c, r) color =
  set c r (Cell '+' fg (attr color))
 where
  fg =
    case color of
      Black -> white
      _     -> black

renderEasel :: Cells
renderEasel =
  mconcat
    [ foldMap (\r -> set ccol (crow+cheight+r) (Cell '│' white black)) [4..7]
    , foldMap (\r -> set (ccol+cwidth-1) (crow+cheight+r) (Cell '│' white black)) [4..7]
    , foldMap (\c -> set (ccol+c+1) (crow+cheight+3) (Cell '─' white black)) [0..cwidth-2]
    , foldMap (\c -> set (ccol+c+1) (crow+cheight+8) (Cell '─' white black)) [0..cwidth-2]
    , set ccol (crow+cheight+3) (Cell '┌' white black)
    , set ccol (crow+cheight+8) (Cell '└' white black)
    , set (ccol+cwidth-1) (crow+cheight+3) (Cell '┐' white black)
    , set (ccol+cwidth-1) (crow+cheight+8) (Cell '┘' white black)
    , foldMap
        (\(i, c) ->
          let
            w = 6 :: Int
            h = 3 :: Int
          in
            rect (ccol+1+i*w) (crow+cheight+h+1) w h (attr c))
        (List.zip [0..] [minBound..maxBound])
    ]

renderSelected :: Color -> Cells
renderSelected color =
  text
    (ccol + fromEnum color * 6 + 3)
    (crow+cheight+7)
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
  Black   -> black
