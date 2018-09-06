{-# LANGUAGE TemplateHaskell #-}

module Bha.Game.Impl.Snake
  ( game
  ) where

import Data.Sequence   (pattern (:|>), Seq, (|>))

import Bha.Elm.Prelude

import qualified Data.Sequence as Seq


--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

type Row = Int
type Col = Int

data Direction
  = DirUp
  | DirDown
  | DirLeft
  | DirRight
  deriving (Eq)

data Model
  = Model
  { _modelSnakeL     :: !(Seq (Col, Row))
  , _modelDirL       :: !Direction
  , _modelFoodL      :: !(Col, Row)
  , _modelScoreL     :: !Int
  , _modelHighScoreL :: !(Maybe Int)
  , _modelPauseL     :: !Bool
  , _modelAliveL     :: !Bool
  }
makeFields ''Model

rmax :: Row
rmax = 20

cmax :: Col
cmax = 40

init :: Init Model
init = do
  highScore :: Maybe Int <-
    load "highScore"

  food <- randomCell

  pure Model
    { _modelSnakeL     = pure (0, 0)
    , _modelDirL       = DirRight
    , _modelFoodL      = food
    , _modelScoreL     = 0
    , _modelHighScoreL = highScore
    , _modelPauseL     = False
    , _modelAliveL     = True
    }


--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------

update :: Either NominalDiffTime Event -> Update Model ()
update event = do
  model <- get

  if
    -- When dead, <Esc> saves and quits the game.
    | not (model ^. aliveL) ->
        case event of
          Right (EventKey KeyEsc _) -> do
            when (Just (model ^. scoreL) > model ^. highScoreL)
              (save "highScore" (model ^. scoreL))
            gameover

          _ ->
            pure ()

    -- When paused <Space> unpauses the game.
    | model ^. pauseL ->
        case event of
          Right (EventKey KeySpace _) ->
            pauseL .= False

          _ ->
            pure ()

    | otherwise ->
        case event of
          Left _ ->
            updateTick

          Right (EventKey KeyEsc _) ->
            gameover

          Right (EventKey KeyArrowUp _) -> do
            dir <- use dirL
            dirL .= DirUp
            when (dir == DirDown) (snakeL %= Seq.reverse)

          Right (EventKey KeyArrowDown _) -> do
            dir <- use dirL
            dirL .= DirDown
            when (dir == DirUp) (snakeL %= Seq.reverse)

          Right (EventKey KeyArrowLeft _) -> do
            dir <- use dirL
            dirL .= DirLeft
            when (dir == DirRight) (snakeL %= Seq.reverse)

          Right (EventKey KeyArrowRight _) -> do
            dir <- use dirL
            dirL .= DirRight
            when (dir == DirLeft) (snakeL %= Seq.reverse)

          Right (EventKey KeySpace _) ->
            pauseL .= True

          Right _ ->
            pure ()

updateTick :: Update Model ()
updateTick = do
  model :: Model <-
    get

  let
    snake =
      model ^. snakeL

  let
    (headCol, headRow) =
      case snake of
        _ :|> x -> x
        _       -> error "empty snake"

    target :: (Col, Row)
    target =
      case model ^. dirL of
        DirUp    -> (headCol,   headRow-1)
        DirDown  -> (headCol,   headRow+1)
        DirLeft  -> (headCol-1, headRow)
        DirRight -> (headCol+1, headRow)

  let
    isDead :: Bool
    isDead =
      target `elem` Seq.drop 1 snake || not (inBounds target)

  if isDead
  then
    aliveL .= False
  else do
    if model ^. foodL == target
    then do
      scoreL += 1

      let
        newSnake =
          (snake |> target)

      snakeL .= newSnake

      if length newSnake == cmax*rmax
      then do
        aliveL .= False
      else do
        newFood <-
          fix $ \loop -> do
            food <- randomCell
            if food `elem` newSnake
              then loop
              else pure food
        foodL .= newFood
    else
      snakeL %= (Seq.drop 1 >>> (|> target))

inBounds :: (Col, Row) -> Bool
inBounds (col, row) =
  and [ col >= 0, col < cmax, row >= 0, row < rmax ]

randomCell :: MonadElm m => m (Col, Row)
randomCell =
  (,) <$> randomInt 0 (cmax-1) <*> randomInt 0 (rmax-1)


--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

view :: Model -> Scene
view model =
  Scene cells NoCursor
 where
  cells :: Cells
  cells =
    (mconcat . catMaybes)
      [ Just (viewBorder (model ^. aliveL))
      , Just (viewSnake (model ^. snakeL))
      , Just (viewFood (model ^. foodL))
      , Just (viewScore (model ^. scoreL))
      , viewHighScore <$> (model ^. highScoreL)
      ]

viewBorder :: Bool -> Cells
viewBorder alive =
  mconcat
    [ foldMap (\c -> set c  0  (Cell ' ' mempty bg)) [0..41]
    , foldMap (\c -> set c  21 (Cell ' ' mempty bg)) [0..41]
    , foldMap (\r -> set 0  r  (Cell ' ' mempty bg)) [1..20]
    , foldMap (\r -> set 41 r  (Cell ' ' mempty bg)) [1..20]
    ]
 where
  bg = if alive then green else red

viewSnake :: Seq (Col, Row) -> Cells
viewSnake =
  foldMap (\(col, row) -> set (col+1) (row+1) (Cell ' ' mempty white))

viewFood :: (Col, Row) -> Cells
viewFood (col, row) =
  set (col+1) (row+1) (Cell ' ' mempty blue)

viewScore :: Int -> Cells
viewScore score =
  tbstr 0 22 mempty mempty ("Score: " ++ show score)

viewHighScore :: Int -> Cells
viewHighScore score =
  tbstr 0 23 mempty mempty ("High score: " ++ show score)


--------------------------------------------------------------------------------
-- Tick
--------------------------------------------------------------------------------

tickEvery :: Model -> Maybe NominalDiffTime
tickEvery model = do
  guard (not (model ^. pauseL))
  guard (model ^. aliveL)

  let
    score = model ^. scoreL
    dir = model ^. dirL

  case dir of
    DirUp    -> Just (1 / 14 * (1 + fromIntegral score / 100))
    DirDown  -> Just (1 / 14 * (1 + fromIntegral score / 100))
    DirLeft  -> Just (1 / 20 * (1 + fromIntegral score / 100))
    DirRight -> Just (1 / 20 * (1 + fromIntegral score / 100))


--------------------------------------------------------------------------------
-- Game
--------------------------------------------------------------------------------

game :: ElmGame Model
game =
  ElmGame init update view tickEvery
