{-# LANGUAGE LambdaCase, NoImplicitPrelude, ScopedTypeVariables #-}

module Bha.Game.Impl.H2048
  ( moment
  ) where

import Data.List (transpose)

import Bha.Banana.Prelude

-- TODO 2048 detect game over
-- TODO 2048 keep score
moment :: Events TermEvent -> Banana (Behavior Scene, Events ())
moment eEvent = do
  let
    eUp    = filterE (== EventKey KeyArrowUp    False) eEvent
    eDown  = filterE (== EventKey KeyArrowDown  False) eEvent
    eLeft  = filterE (== EventKey KeyArrowLeft  False) eEvent
    eRight = filterE (== EventKey KeyArrowRight False) eEvent
    eEsc   = filterE (== EventKey KeyEsc        False) eEvent

  let
    eDone :: Events ()
    eDone =
      () <$ eEsc

  -- TODO 2048 add new tile after every move
  bBoard :: Behavior [[Maybe Int]] <-
    accumB initialBoard $ unions
      -- TODO 2048 hjkl controls
      [ boardUp    <$ eUp
      , boardDown  <$ eDown
      , boardLeft  <$ eLeft
      , boardRight <$ eRight
      ]

  let
    bCells :: Behavior Cells
    bCells =
      renderBoard <$> bBoard

  let
    bScene :: Behavior Scene
    bScene =
      Scene
        <$> bCells
        <*> pure NoCursor

  pure (bScene, eDone)

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

renderBoard :: [[Maybe Int]] -> Cells
renderBoard =
  foldMap (uncurry renderRow) . zip [0..]

renderRow :: Int -> [Maybe Int] -> Cells
renderRow row =
  foldMap (uncurry (renderCell row)) . zip [0..]

renderCell :: Int -> Int -> Maybe Int -> Cells
renderCell row0 col0 mx =
  mconcat
    [ tbstr col row     mempty mempty "+------"
    , tbstr col (row+1) mempty mempty
        (case mx of
          Nothing -> "|"
          Just x  -> "|" ++ show x)
    , tbstr col (row+2) mempty mempty "|"
    ]
 where
  col = 0 + col0*7
  row = 0 + row0*3

--------------------------------------------------------------------------------
-- Board manipulation
--------------------------------------------------------------------------------

-- TODO 2048 random initial board
initialBoard :: [[Maybe Int]]
initialBoard =
  [ [ Nothing, Nothing, Just 2  ]
  , [ Nothing, Nothing, Nothing ]
  , [ Nothing, Nothing, Just 2  ]
  , [ Nothing, Nothing, Nothing ]
  ]

boardLeft :: [[Maybe Int]] -> [[Maybe Int]]
boardLeft =
  map rowLeft

boardRight :: [[Maybe Int]] -> [[Maybe Int]]
boardRight =
  map (reverse . rowLeft . reverse)

boardUp :: [[Maybe Int]] -> [[Maybe Int]]
boardUp =
  transpose . boardLeft . transpose

boardDown :: [[Maybe Int]] -> [[Maybe Int]]
boardDown =
  transpose . boardRight . transpose

rowLeft :: [Maybe Int] -> [Maybe Int]
rowLeft xs0 =
  (take (length xs0) . (++ repeat Nothing) . map Just . go . catMaybes) xs0
 where
  go :: [Int] -> [Int]
  go = \case
    x:y:ys | x == y ->
      (x+y) : go ys
    x:xs ->
      x : go xs
    [] ->
      []
