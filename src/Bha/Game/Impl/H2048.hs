module Bha.Game.Impl.H2048
  ( moment
  ) where

import Bha.Banana.Prelude
import Bha.Banana.Versioned
import Bha.Data.Board       (Board)

import qualified Bha.Data.Board as Board

import Data.List   (intercalate, sortOn)
import Data.Monoid (Sum(..))
import Data.Ord    (Down(Down))
import Data.Tuple  (uncurry)


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
      load "highScore"

  let
    eUp    = filterE ((||) <$> isKeyArrowUp <*> isKeyChar 'k') eEvent
    eDown  = filterE ((||) <$> isKeyArrowDown <*> isKeyChar 'j') eEvent
    eLeft  = filterE ((||) <$> isKeyArrowLeft <*> isKeyChar 'h') eEvent
    eRight = filterE ((||) <$> isKeyArrowRight <*> isKeyChar 'l') eEvent

    eEsc = filterE isKeyEsc eEvent
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
    eBoardUp :: Events (Board Int)
    eBoardUp    = mapMaybeE (ifChanged boardUp)    (bBoard <@ eUDLR)
    eBoardDown  = mapMaybeE (ifChanged boardDown)  (bBoard <@ eUDLR)
    eBoardLeft  = mapMaybeE (ifChanged boardLeft)  (bBoard <@ eUDLR)
    eBoardRight = mapMaybeE (ifChanged boardRight) (bBoard <@ eUDLR)

  eBoard :: Events (Board Int) <- do
    let
      plus1 :: Events (Board Int) -> Banana (Events (Board Int))
      plus1 =
        fmap f >>> executeE
       where
        f :: Board Int -> Banana (Board Int)
        f board = do
          (r, c) <- randomOneOf (Board.holes board)
          pct <- randomPct
          pure (board & Board.cell r c .~ (if pct >= 0.9 then Just 4 else Just 2))

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

  bBoard :: Behavior (Board Int) <- do
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

  save "highScore"
    (((:highScores) >>> sortOn Down >>> take 10 >>> HighScores)
      <$> bScore
      <@  eDone)

  pure (bScene, pure mempty, never, eDone)


--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

renderBoard :: Board Int -> Cells
renderBoard cells = do
  mconcat
    [ (zip [0..] >>> foldMap (uncurry renderRow)) cells
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
  Nothing   -> rect' col row 8 4 (Cell '•' white black)
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

initialBoard :: Banana (Board Int)
initialBoard = do
  n <- randomInt 0 15
  pure (chunksOf 4 (map (\i -> if i == n then Just 2 else Nothing) [0..15]))

boardLeft :: Board Int -> Board Int
boardLeft =
  Board.rows %~ rowLeft

boardRight :: Board Int -> Board Int
boardRight =
  Board.rows %~ rowRight

boardUp :: Board Int -> Board Int
boardUp =
  Board.cols %~ rowLeft

boardDown :: Board Int -> Board Int
boardDown =
  Board.cols %~ rowRight

boardScore :: Board Int -> Int
boardScore =
  getSum . foldMapOf Board.elems Sum

rowLeft :: Board.Row Int -> Board.Row Int
rowLeft xs0 =
  (catMaybes >>> go >>> map Just >>> (++ repeat Nothing) >>> take (length xs0)) xs0
 where
  go :: [Int] -> [Int]
  go = \case
    x:y:ys | x == y ->
      (x+y) : go ys
    x:xs ->
      x : go xs
    [] ->
      []

rowRight :: Board.Row Int -> Board.Row Int
rowRight =
  reverse >>> rowLeft >>> reverse

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
