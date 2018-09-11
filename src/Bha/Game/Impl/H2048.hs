module Bha.Game.Impl.H2048
  ( moment
  ) where

import Data.List       (intercalate, sortOn, transpose)
import Data.Ord        (Down(Down))
import System.FilePath ((</>))

import Bha.Banana.Prelude
import Bha.Banana.Versioned

newtype HighScores
  = HighScores [Int]
  deriving stock (Generic)
  deriving anyclass (Serialize, Versioned '[])

moment
  :: Events (Text, Void)
  -> Events TermEvent
  -> Banana
       ( Behavior Scene
       , Behavior (HashSet Text)
       , Events (Text, Void)
       , Events ()
       )
moment _ eEvent = mdo
  HighScores highScores <-
    fromMaybe (HighScores []) <$>
      load (bhaDataDir </> "2048" </> "highScore")

  let
    eUp    = filterE ((||) <$> (== EventKey KeyArrowUp    False) <*> (== EventKey (KeyChar 'k') False)) eEvent
    eDown  = filterE ((||) <$> (== EventKey KeyArrowDown  False) <*> (== EventKey (KeyChar 'j') False)) eEvent
    eLeft  = filterE ((||) <$> (== EventKey KeyArrowLeft  False) <*> (== EventKey (KeyChar 'h') False)) eEvent
    eRight = filterE ((||) <$> (== EventKey KeyArrowRight False) <*> (== EventKey (KeyChar 'l') False)) eEvent

    eEsc = filterE (== EventKey KeyEsc False) eEvent
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
    eBoardUp    = mapMaybeE (ifChanged boardUp)    (bBoard <@ eUDLR)
    eBoardDown  = mapMaybeE (ifChanged boardDown)  (bBoard <@ eUDLR)
    eBoardLeft  = mapMaybeE (ifChanged boardLeft)  (bBoard <@ eUDLR)
    eBoardRight = mapMaybeE (ifChanged boardRight) (bBoard <@ eUDLR)

  eBoard :: Events [[Maybe Int]] <- do
    let
      plus1 :: Events [[Maybe Int]] -> Banana (Events [[Maybe Int]])
      plus1 =
        executeE . fmap f
       where
        f :: [[Maybe Int]] -> Banana [[Maybe Int]]
        f board = do
          coord <- randomOneOf (boardHoles board)
          pct <- randomPct
          pure (boardSet coord (if pct >= 0.9 then 4 else 2) board)

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

  bBoard :: Behavior [[Maybe Int]] <- do
    board0 <- initialBoard
    stepper board0 eBoard

  let
    bScore :: Behavior Int
    bScore =
      boardScore <$> bBoard

  let
    bCells :: Behavior Cells
    bCells =
      mconcat
        [ renderBoard <$> bBoard
        , renderScore <$> bScore
        , pure (renderHighScores highScores)
        ]

  let
    bScene :: Behavior Scene
    bScene =
      Scene
        <$> bCells
        <*> pure NoCursor

  reactimate
    ((\score ->
      save (bhaDataDir </> "2048") "highScore"
        (HighScores (take 10 (sortOn Down (score:highScores)))))
    <$> bScore
    <@  eDone)

  pure (bScene, pure mempty, never, eDone)

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

renderBoard :: [[Maybe Int]] -> Cells
renderBoard cells = do
  mconcat
    [ (foldMap (uncurry renderRow) . zip [0..]) cells
    , renderBorder
    ]

renderBorder :: Cells
renderBorder =
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

renderRow :: Int -> [Maybe Int] -> Cells
renderRow row =
  foldMap (uncurry (renderCell row)) . zip [0..]

renderCell :: Int -> Int -> Maybe Int -> Cells
renderCell row0 col0 = \case
  Nothing   -> rect' col row 8 4 (Cell '‧' white black)
  Just 2    -> rect  col row 8 4 255 -- yellow
  Just 4    -> rect  col row 8 4 249 -- tangerine
  Just 8    -> rect  col row 8 4 243 -- orange
  Just 16   -> rect  col row 8 4 237 -- red
  Just 32   -> rect  col row 8 4 yellow -- magenta
  Just 64   -> rect  col row 8 4 tangerine -- green
  Just 128  -> rect  col row 8 4 red -- cyan
  Just 256  -> rect  col row 8 4 magenta -- blue
  Just 512  -> rect  col row 8 4 green -- 30
  Just 1024 -> rect  col row 8 4 cyan -- 104
  Just 2048 -> rect  col row 8 4 blue -- 7
  Just 4096 -> rect  col row 8 4 21 -- 20
  Just _    -> rect  col row 8 4 15 -- 8

 where
  col = 1 + col0*8
  row = 1 + row0*4

renderScore :: Int -> Cells
renderScore n =
  text 0 18 mempty mempty ("Score: " ++ show n)

renderHighScores :: [Int] -> Cells
renderHighScores [] = mempty
renderHighScores ns =
  text 0 19 mempty mempty ("High scores: " ++ intercalate ", " (map show ns))

--------------------------------------------------------------------------------
-- Board manipulation
--------------------------------------------------------------------------------

type Col = Int
type Row = Int

initialBoard :: Banana [[Maybe Int]]
initialBoard = do
  n <- randomInt 0 15
  pure (chunksOf 4 (map (\i -> if i == n then Just 2 else Nothing) [0..15]))

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

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs =
  let
    (ys, zs) =
      splitAt n xs
  in
    case zs of
      [] -> [ys]
      _  -> ys : chunksOf n zs
