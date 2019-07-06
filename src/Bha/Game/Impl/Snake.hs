{-# LANGUAGE TemplateHaskell #-}

module Bha.Game.Impl.Snake
  ( game
  ) where

import Bha.Elm.Prelude
import Bha.Elm.Versioned

import Data.Sequence (pattern (:|>), (|>))

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
  deriving stock (Eq, Show)

newtype HighScore
  = HighScore { unHighScore :: Int }
  deriving stock (Generic)
  deriving anyclass (Serialize, Versioned '[])

data Model
  = Model
  { snake     :: (Seq (Col, Row))
  , dir       :: Direction
  , food      :: (Col, Row)
  , score     :: Int
  , highScore :: (Maybe Int)
  , pause     :: Bool
  , alive     :: Bool
  } deriving stock (Generic, Show)

rmax :: Row
rmax = 20

cmax :: Col
cmax = 40

init :: Int -> Int -> Init Void Model
init _ _ = do
  highScore :: Maybe Int <-
    (fmap.fmap) unHighScore (load "highScore")

  food <- randomCell

  pure Model
    { snake     = pure (0, 0)
    , dir       = DirRight
    , food      = food
    , score     = 0
    , highScore = highScore
    , pause     = False
    , alive     = True
    }


--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------

update :: Input Void -> Update Model Void ()
update input = do
  model <- get

  if
    -- When dead, <Esc> saves and quits the game.
    | not (model ^. #alive) ->
        case input of
          Key KeyEsc -> do
            when (Just (model ^. #score) > model ^. #highScore)
              (save "highScore" (HighScore (model ^. #score)))
            gameover

          _ ->
            pure ()

    -- When paused <Space> unpauses the game.
    | model ^. #pause ->
        case input of
          Key KeySpace ->
            #pause .= False

          _ ->
            pure ()

    | otherwise ->
        case input of
          Tick _ ->
            updateTick

          Key KeyEsc ->
            gameover

          Key KeyArrowUp -> do
            dir <- use #dir
            #dir .= DirUp
            when (dir == DirDown) (#snake %= Seq.reverse)

          Key KeyArrowDown -> do
            dir <- use #dir
            #dir .= DirDown
            when (dir == DirUp) (#snake %= Seq.reverse)

          Key KeyArrowLeft -> do
            dir <- use #dir
            #dir .= DirLeft
            when (dir == DirRight) (#snake %= Seq.reverse)

          Key KeyArrowRight -> do
            dir <- use #dir
            #dir .= DirRight
            when (dir == DirLeft) (#snake %= Seq.reverse)

          Key KeySpace ->
            #pause .= True

          _ ->
            pure ()

updateTick :: Update Model Void ()
updateTick = do
  model :: Model <-
    get

  let
    snake =
      model ^. #snake

  let
    (headCol, headRow) =
      case snake of
        _ :|> x -> x
        _       -> error "empty snake"

    target :: (Col, Row)
    target =
      case model ^. #dir of
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
    #alive .= False
  else do
    if model ^. #food == target
    then do
      #score %= (+1)

      let
        newSnake =
          (snake |> target)

      #snake .= newSnake

      if length newSnake == cmax*rmax
      then do
        #alive .= False
      else do
        newFood <-
          fix $ \loop -> do
            food <- randomCell
            if food `elem` newSnake
              then loop
              else pure food
        #food .= newFood
    else
      #snake %= (Seq.drop 1 >>> (|> target))

inBounds :: (Col, Row) -> Bool
inBounds (col, row) =
  and [ col >= 0, col < cmax, row >= 0, row < rmax ]

randomCell :: MonadElm message m => m (Col, Row)
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
      [ Just (viewBorder (model ^. #alive))
      , Just (viewSnake (model ^. #snake))
      , Just (viewFood (model ^. #food))
      , Just (viewScore (model ^. #score))
      , viewHighScore <$> (model ^. #highScore)
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
  text 0 22 mempty mempty ("Score: " ++ show score)

viewHighScore :: Int -> Cells
viewHighScore score =
  text 0 23 mempty mempty ("High score: " ++ show score)


--------------------------------------------------------------------------------
-- Tick
--------------------------------------------------------------------------------

tickEvery :: Model -> Maybe Seconds
tickEvery model = do
  guard (not (model ^. #pause))
  guard (model ^. #alive)

  let
    score = model ^. #score
    dir = model ^. #dir

  case dir of
    DirUp    -> Just (1 / 14 * (1 - fromIntegral score / 100))
    DirDown  -> Just (1 / 14 * (1 - fromIntegral score / 100))
    DirLeft  -> Just (1 / 20 * (1 - fromIntegral score / 100))
    DirRight -> Just (1 / 20 * (1 - fromIntegral score / 100))



--------------------------------------------------------------------------------
-- Subscribe
--------------------------------------------------------------------------------

subscribe :: Model -> HashSet Text
subscribe _ =
  mempty


--------------------------------------------------------------------------------
-- Game
--------------------------------------------------------------------------------

game :: ElmGame Model Void
game =
  ElmGame init update view tickEvery subscribe
