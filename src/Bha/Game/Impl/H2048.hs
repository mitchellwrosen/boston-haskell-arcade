{-# LANGUAGE LambdaCase, NoImplicitPrelude, RecursiveDo, ScopedTypeVariables #-}

module Bha.Game.Impl.H2048
  ( moment
  ) where

import Data.List (transpose)

import Bha.Banana.Prelude

-- TODO 2048 detect game over
-- TODO 2048 keep score
moment :: Events TermEvent -> Banana (Behavior Scene, Events ())
moment eEvent = mdo
  -- TODO 2048 hjkl controls
  let
    eUp    = filterE (== EventKey KeyArrowUp    False) eEvent
    eDown  = filterE (== EventKey KeyArrowDown  False) eEvent
    eLeft  = filterE (== EventKey KeyArrowLeft  False) eEvent
    eRight = filterE (== EventKey KeyArrowRight False) eEvent
    eEsc   = filterE (== EventKey KeyEsc        False) eEvent

    eUDLR  = leftmostE [eUp, eDown, eLeft, eRight]

  let
    eDone :: Events ()
    eDone =
      (() <$ eEsc) <> eGameOver

  let
    eGameOver :: Events ()
    eGameOver =
      filterJust
        (mergeE
          (\_ -> Just ())
          (\_ -> Nothing)
          (\_ _ -> Nothing)
          eUDLR
          (leftmostE [eBoardUp, eBoardDown, eBoardLeft, eBoardRight]))

  let
    eBoardUp :: Events [[Maybe Int]]
    eBoardUp    = filterJust (ifChanged boardUp    <$> bBoard <@ eUDLR)
    eBoardDown  = filterJust (ifChanged boardDown  <$> bBoard <@ eUDLR)
    eBoardLeft  = filterJust (ifChanged boardLeft  <$> bBoard <@ eUDLR)
    eBoardRight = filterJust (ifChanged boardRight <$> bBoard <@ eUDLR)

  eBoard :: Events [[Maybe Int]] <- do
    let
      plus1 :: Events [[Maybe Int]] -> Banana (Events [[Maybe Int]])
      plus1 =
        executeE . fmap f
       where
        f :: [[Maybe Int]] -> Banana [[Maybe Int]]
        f board = do
          coord <- randomOneOf (boardHoles board)
          pure (boardSet coord 2 board)

    eBoardUp'    <- plus1 (filterCoincidentE eUp    eBoardUp)
    eBoardDown'  <- plus1 (filterCoincidentE eDown  eBoardDown)
    eBoardLeft'  <- plus1 (filterCoincidentE eLeft  eBoardLeft)
    eBoardRight' <- plus1 (filterCoincidentE eRight eBoardRight)

    pure
      (leftmostE
        [ eBoardUp'
        , eBoardDown'
        , eBoardLeft'
        , eBoardRight'
        ])

  bBoard :: Behavior [[Maybe Int]] <-
    stepper initialBoard eBoard

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

type Col = Int
type Row = Int

-- TODO 2048 random initial board
initialBoard :: [[Maybe Int]]
initialBoard =
  [ [ Nothing, Nothing, Just 2,  Nothing ]
  , [ Nothing, Nothing, Nothing, Nothing ]
  , [ Nothing, Nothing, Just 2,  Nothing ]
  , [ Nothing, Nothing, Nothing, Nothing ]
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

boardHoles :: [[Maybe Int]] -> [(Row, Col)]
boardHoles board = do
  (r, xs) <- zip [0..] board
  (c, x) <- zip [0..] xs
  guard (isNothing x)
  pure (r, c)

boardSet :: (Row, Col) -> Int -> [[Maybe Int]] -> [[Maybe Int]]
boardSet (r, c) n =
  ix r %~ (ix c .~ Just n)

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

ifChanged :: Eq a => (a -> a) -> (a -> Maybe a)
ifChanged f x = do
  guard (x /= y)
  Just y
 where
  y = f x
