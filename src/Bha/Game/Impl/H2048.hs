module Bha.Game.Impl.H2048
  ( moment
  ) where

import Data.List       (transpose)
import System.FilePath ((</>))

import Bha.Banana.Prelude

moment
  :: Events TermEvent
  -> Banana (Behavior Scene, Events ())
moment eEvent = mdo
  highScore :: Maybe Int <-
    load (bhaDataDir </> "2048" </> "highScore")

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
      leftmostE
        [ () <$ eEsc
        , eGameOver
        ]
     where
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
    bScore :: Behavior Int
    bScore =
      boardScore <$> bBoard

  let
    bCells :: Behavior Cells
    bCells =
      (mconcat . catMaybes)
        [ Just (renderBoard <$> bBoard)
        , Just (renderScore <$> bScore)
        , pure . renderHighScore <$> highScore
        ]

  let
    bScene :: Behavior Scene
    bScene =
      Scene
        <$> bCells
        <*> pure NoCursor

  reactimate
    ((\score ->
      when (Just score > highScore)
        (save (bhaDataDir </> "2048") "highScore" score))
    <$> bScore
    <@  eDone)
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
renderCell row0 col0 = \case
  Nothing   -> rect  col row 8 4 mempty black
  Just 2    -> rect  col row 8 4 mempty blue
  Just 4    -> rect  col row 8 4 mempty red
  Just 8    -> rect  col row 8 4 mempty yellow
  Just 16   -> rect  col row 8 4 mempty green
  Just 32   -> rect  col row 8 4 mempty magenta
  Just 64   -> rect  col row 8 4 mempty cyan
  Just 128  -> rects col row 4 2 mempty blue red
  Just 256  -> rects col row 4 2 mempty red yellow
  Just 512  -> rects col row 4 2 mempty yellow green
  Just 1024 -> rects col row 4 2 mempty green magenta
  Just 2048 -> rects col row 4 2 mempty magenta cyan
  Just 4096 -> rects col row 4 2 mempty cyan black
  Just _    -> rect  col row 8 4 mempty black

 where
  col = 0 + col0*8
  row = 0 + row0*4

  rects c r w h fg bg1 bg2 =
    mconcat
      [ rect c     r     w h fg bg1
      , rect (c+4) r     w h fg bg2
      , rect c     (r+2) w h fg bg2
      , rect (c+4) (r+2) w h fg bg1
      ]

renderScore :: Int -> Cells
renderScore n =
  tbstr 0 16 mempty mempty ("Score: " ++ show n)

renderHighScore :: Int -> Cells
renderHighScore n =
  tbstr 0 17 mempty mempty ("High score: " ++ show n)

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

boardScore :: [[Maybe Int]] -> Int
boardScore board = sum $ do
  row <- board
  Just n <- row
  pure n

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
