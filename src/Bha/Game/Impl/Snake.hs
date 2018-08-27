{-# LANGUAGE FlexibleInstances, FunctionalDependencies, LambdaCase,
             MultiParamTypeClasses, NamedFieldPuns, NoImplicitPrelude,
             PatternSynonyms, RecordWildCards, ScopedTypeVariables,
             TemplateHaskell #-}

module Bha.Game.Impl.Snake
  ( game
  ) where

import Bha.Elm.Prelude

import Data.Sequence (pattern (:|>), Seq, (|>))

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
  { _modelSeedL  :: Seed
  , _modelSnakeL :: Seq (Col, Row)
  , _modelDirL   :: Direction
  , _modelFoodL  :: (Col, Row)
  }
makeFields ''Model

rmax :: Row
rmax = 20

cmax :: Col
cmax = 40

init :: Seed -> Model
init seed =
  Model seed (pure (0, 0)) DirRight (5, 5)


--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------

update :: Either NominalDiffTime Event -> StateT Model Maybe ()
update = \case
  Left _ ->
    updateTick

  Right (EventKey KeyEsc _) ->
    empty

  Right (EventKey KeyArrowUp _) -> do
    dir <- use dirL
    unless (dir == DirDown) (dirL .= DirUp)

  Right (EventKey KeyArrowDown _) -> do
    dir <- use dirL
    unless (dir == DirUp) (dirL .= DirDown)

  Right (EventKey KeyArrowLeft _) -> do
    dir <- use dirL
    unless (dir == DirRight) (dirL .= DirLeft)

  Right (EventKey KeyArrowRight _) -> do
    dir <- use dirL
    unless (dir == DirLeft) (dirL .= DirRight)

  Right _ ->
    pure ()

updateTick :: StateT Model Maybe ()
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

  guard (inBounds target)
  guard (target `notElem` Seq.drop 1 snake)

  if model ^. foodL == target
    then do
      guard (length snake < cmax*rmax - 1)

      let
        newSnake =
          snake |> target

      newFood <-
        fix $ \loop -> do
          food <- zoom seedL randomFood
          if food `elem` newSnake
            then loop
            else pure food


      snakeL .= newSnake
      foodL  .= newFood

    else
      snakeL %= (Seq.drop 1 >>> (|> target))

inBounds :: (Col, Row) -> Bool
inBounds (col, row) =
  and [ col >= 0, col < cmax, row >= 0, row < rmax ]

randomFood :: Monad m => StateT Seed m (Col, Row)
randomFood =
  ((,)
    <$> randomIntS 0 (cmax-1)
    <*> randomIntS 0 (rmax-1))


--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

view :: Model -> Scene
view model =
  Scene cells NoCursor
 where
  cells :: Cells
  cells =
    mconcat
      [ viewBorder
      , viewSnake (model ^. snakeL)
      , viewFood (model ^. foodL)
      ]

viewBorder :: Cells
viewBorder =
  mconcat
    [ foldMap (\c -> set c  0  (Cell ' ' mempty green)) [0..41]
    , foldMap (\c -> set c  21 (Cell ' ' mempty green)) [0..41]
    , foldMap (\r -> set 0  r  (Cell ' ' mempty green)) [1..20]
    , foldMap (\r -> set 41 r  (Cell ' ' mempty green)) [1..20]
    ]

viewSnake :: Seq (Col, Row) -> Cells
viewSnake =
  foldMap (\(col, row) -> set (col+1) (row+1) (Cell ' ' mempty white))

viewFood :: (Col, Row) -> Cells
viewFood (col, row) =
  set (col+1) (row+1) (Cell ' ' mempty blue)


--------------------------------------------------------------------------------
-- Tick
--------------------------------------------------------------------------------

tickEvery :: Model -> Maybe NominalDiffTime
tickEvery model =
  case model ^. dirL of
    DirUp    -> Just (1/14)
    DirDown  -> Just (1/14)
    DirLeft  -> Just (1/20)
    DirRight -> Just (1/20)


--------------------------------------------------------------------------------
-- Game
--------------------------------------------------------------------------------

game :: ElmGame Model
game =
  ElmGame init update view tickEvery
