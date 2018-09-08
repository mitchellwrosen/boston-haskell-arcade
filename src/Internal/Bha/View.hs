module Internal.Bha.View
  ( Cells
  , set
  , Scene(..)
  , sceneToTbScene
  , cursorColL
  , cursorRowL
  , text
  , rect
  , rect'
  ) where

import Termbox.Banana (Attr, Cell(..), Cursor(..))

import qualified Data.HashMap.Strict as HashMap
import qualified Termbox.Banana as Tb

import Bha.Prelude

newtype Cells
  = Cells (HashMap (Int, Int) Cell)

instance Monoid Cells where
  mempty = Cells mempty
  mappend = (<>)

instance Semigroup Cells where
  Cells x <> Cells y =
    Cells (y <> x)

data Scene
  = Scene !Cells !Cursor

set :: Int -> Int -> Cell -> Cells
set c r x =
  Cells (HashMap.singleton (c, r) x)

cellsToTbCells :: Cells -> Tb.Cells
cellsToTbCells (Cells cells) =
  foldMap (\((c, r), x) -> Tb.set c r x) (HashMap.toList cells)

sceneToTbScene :: Scene -> Tb.Scene
sceneToTbScene (Scene cells cursor) =
  Tb.Scene (cellsToTbCells cells) cursor

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
  foldMap (\(col, c) -> set col row (Cell c fg bg)) . zip [col0..]

rect :: Int -> Int -> Int -> Int -> Attr -> Cells
rect c0 r0 w h bg =
  foldMap
    (\(r, c) -> set c r (Cell ' ' mempty bg))
    ((,)
      <$> [r0 .. r0 + h - 1]
      <*> [c0 .. c0 + w - 1])

-- TODO only one rect function?
rect' :: Int -> Int -> Int -> Int -> Cell -> Cells
rect' c0 r0 w h cell =
  foldMap
    (\(r, c) -> set c r cell)
    ((,)
      <$> [r0 .. r0 + h - 1]
      <*> [c0 .. c0 + w - 1])
