-- | Example Elm-style game.

{-# LANGUAGE NamedFieldPuns, NoImplicitPrelude, PatternSynonyms,
             RecordWildCards #-}

module Bha.Game.Impl.Snake
  ( game
  ) where

import Bha.Elm.Prelude
-- import Bha.View

import Control.Monad.Trans.State
import Data.List                 (unfoldr)
import Data.Maybe                (fromJust)
import Data.Sequence             (pattern (:|>), Seq, (|>))

import qualified Data.Sequence as Seq

type Row = Int
type Col = Int

rmax :: Row
rmax = 20

cmax :: Col
cmax = 40

data Direction
  = DirUp
  | DirDown
  | DirLeft
  | DirRight
  deriving (Eq)

data Model
  = Model
  { modelSeed  :: StdGen
  , modelSnake :: Seq (Col, Row)
  , modelDir   :: Direction
  , modelFood  :: (Col, Row)
  }

game :: ElmGame
game =
  ElmGame init update view tickEvery

init :: StdGen -> Model
init seed =
  Model seed (pure (0, 0)) DirRight (5, 5)

inBounds :: (Col, Row) -> Bool
inBounds (col, row) =
  and [ col >= 0, col < cmax, row >= 0, row < rmax ]

update :: Either NominalDiffTime Event -> Model -> Maybe Model
update event model =
  case event of
    Left _ ->
      updateTick model

    Right (EventKey KeyEsc _) ->
      Nothing

    Right (EventKey KeyArrowUp _) ->
      if modelDir model == DirDown
        then Just model
        else Just model { modelDir = DirUp }

    Right (EventKey KeyArrowDown _) ->
      if modelDir model == DirUp
        then Just model
        else Just model { modelDir = DirDown }

    Right (EventKey KeyArrowLeft _) ->
      if modelDir model == DirRight
        then Just model
        else Just model { modelDir = DirLeft }

    Right (EventKey KeyArrowRight _) ->
      if modelDir model == DirLeft
        then Just model
        else Just model { modelDir = DirRight }

    Right _ ->
      Just model

updateTick :: Model -> Maybe Model
updateTick model =
  let
    (headCol, headRow) =
      case modelSnake model of
        _ :|> x -> x
        _ -> error "empty snake"

    target :: (Col, Row)
    target =
      case modelDir model of
        DirUp    -> (headCol,   headRow-1)
        DirDown  -> (headCol,   headRow+1)
        DirLeft  -> (headCol-1, headRow)
        DirRight -> (headCol+1, headRow)

  in do
    guard (inBounds target)
    guard (target `notElem` Seq.drop 1 (modelSnake model))

    if modelFood model == target
      then do
        guard (length (modelSnake model) < cmax*rmax - 1)

        let
          newSnake :: Seq (Col, Row)
          newSnake =
            modelSnake model |> target

        let
          candidates :: [((Col, Row), StdGen)]
          candidates =
            unfoldr
              (\seed ->
                let
                  x@(_, seed') = randomFood seed
                in
                  Just (x, seed'))
              (modelSeed model)
            where
            randomFood :: StdGen -> ((Col, Row), StdGen)
            randomFood =
              runState
                ((,)
                  <$> ((`mod` cmax) <$> state random)
                  <*> ((`mod` rmax) <$> state random))

        let
          newFood :: (Col, Row)
          newSeed :: StdGen
          (newFood, newSeed) =
            fromJust (find ((`notElem` newSnake) . fst) candidates)

        Just model
          { modelSnake = newSnake
          , modelSeed  = newSeed
          , modelFood  = newFood
          }

      else
        let
          newSnake :: Seq (Col, Row)
          newSnake =
            modelSnake model
              & Seq.drop 1
              & (|> target)
        in
          Just model
            { modelSnake = newSnake
            }


view :: Model -> Scene
view model =
  Scene cells NoCursor
 where
  cells :: Cells
  cells =
    mconcat
      [ viewSnake (modelSnake model)
      , viewFood (modelFood model)
      ]

viewSnake :: Seq (Col, Row) -> Cells
viewSnake =
  foldMap (\(col, row) -> set col row (Cell '-' mempty mempty))

viewFood :: (Col, Row) -> Cells
viewFood (col, row) =
  set col row (Cell 'o' mempty mempty)

tickEvery :: Model -> Maybe NominalDiffTime
tickEvery _ =
  Just (1/10)
