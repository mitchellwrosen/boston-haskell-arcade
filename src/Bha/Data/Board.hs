-- | Generic 2D game board, whose cells may contain a value.

module Bha.Data.Board
  ( Board
  , Row
  , Col
  , cell
  , cells
  , elems
  , row
  , rows
  , col
  , cols
  , holes
  ) where

import Bha.Prelude
import Bha.View    (Cell(..), Cells, black, set, white)

import Data.List (transpose)


-- | A board.
type Board a
  = [Row a]

-- | A row.
type Row a
  = [Maybe a]

-- | A column.
type Col a
  = [Maybe a]

-- | A cell.
cell :: Int -> Int -> Traversal' (Board a) (Maybe a)
cell r c =
  ix r . ix c

-- | The cells of a board.
cells :: Traversal (Board a) (Board b) (Maybe a) (Maybe b)
cells =
  traversed . traversed

-- | The non-empty cells of a board.
elems :: Traversal (Board a) (Board b) a b
elems =
  cells . _Just

-- | A row.
row :: Int -> Traversal' (Board a) (Row a)
row =
  ix

-- | The rows of a board.
rows :: Traversal (Board a) (Board b) (Row a) (Row b)
rows =
  traversed

-- | A column.
col :: Int -> Traversal' (Board a) (Col a)
col n f =
  fmap transpose . ix n f . transpose

-- | The columns of a board.
cols :: Traversal (Board a) (Board b) (Col a) (Col b)
cols f =
  fmap transpose . traverse f . transpose

-- | The indices of the empty cells (row, then col).
holes :: Board a -> [(Int, Int)]
holes board = do
  (r, xs) <- zip [0..] board
  (c, Nothing) <- zip [0..] xs
  pure (r, c)


--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

data RenderBoardOpts
  = RenderBoardOpts
  { cellHeight :: Int
  , cellWidth :: Int
  , topLeftCorner :: (Int, Int) -- ^ Row, col
  }

renderBoard
  :: RenderBoardOpts
  -> (Maybe a -> Cells) -- ^ Cell render function
  -> Board a
  -> Cells
renderBoard opts renderCell board =
  fold
    [ foldMap (uncurry (renderRow opts)) (zip [0..] board)
    , renderBorder opts
    ]

renderRow
  :: RenderBoardOpts
  -> Int
  -> Row a
  -> Cells
renderRow opts rowNum row =
  mempty

renderBorder :: RenderBoardOpts -> Cells
renderBorder opts =
  mconcat
    [ set 0 0 (Cell '┌' white black)
    , set 0 17 (Cell '└' white black)
    , set 33 0 (Cell '┐' white black)
    , set 33 17 (Cell '┘' white black)
    , foldMap (\c -> set c 0 (Cell '─' white black)) [1..32]
    , foldMap (\c -> set c 17 (Cell '─' white black)) [1..32]
    , foldMap (\r -> set 0 r (Cell '│' white black)) [1..16]
    , foldMap (\r -> set 33 r (Cell '│' white black)) [1..16]
    ]

-- renderRow :: Int -> [Maybe Int] -> Cells
-- renderRow row =
--   foldMap (uncurry (renderCell row)) . zip [0..]

-- renderCell :: Int -> Int -> Maybe Int -> Cells
-- renderCell row0 col0 = \case
--   Nothing   -> rect' col row 8 4 (Cell '•' white black)
--   Just 2    -> rect  col row 8 4 255 -- yellow
--   Just 4    -> rect  col row 8 4 249 -- tangerine
--   Just 8    -> rect  col row 8 4 243 -- orange
--   Just 16   -> rect  col row 8 4 237 -- red
--   Just 32   -> rect  col row 8 4 yellow -- magenta
--   Just 64   -> rect  col row 8 4 tangerine -- green
--   Just 128  -> rect  col row 8 4 red -- cyan
--   Just 256  -> rect  col row 8 4 magenta -- blue
--   Just 512  -> rect  col row 8 4 green -- 30
--   Just 1024 -> rect  col row 8 4 cyan -- 104
--   Just 2048 -> rect  col row 8 4 blue -- 7
--   Just 4096 -> rect  col row 8 4 21 -- 20
--   Just _    -> rect  col row 8 4 15 -- 8

--  where
--   col = 1 + col0*8
--   row = 1 + row0*4

-- renderScore :: Int -> Cells
-- renderScore n =
--   text 0 18 mempty mempty ("Score: " ++ show n)

-- renderHighScores :: [Int] -> Cells
-- renderHighScores [] = mempty
-- renderHighScores ns =
--   text 0 19 mempty mempty ("High scores: " ++ intercalate ", " (map show ns))
