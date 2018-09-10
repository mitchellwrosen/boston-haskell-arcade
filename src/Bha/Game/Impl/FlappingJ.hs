module Bha.Game.Impl.FlappingJ
  ( moment
  ) where

import Bha.Banana.Prelude
import Bha.Banana.Tick

moment
  :: Events TermEvent
  -> Banana (Behavior Scene, Events ())
moment eEvent = mdo
  let
    eEsc   = filterE (== EventKey KeyEsc   False) eEvent
    eSpace = filterE (== EventKey KeySpace False) eEvent

    eDone = () <$ eEsc

  eTick :: Events NominalDiffTime <-
    momentTick (Just (1/30)) (TickTeardown <$ eDone)

  let
    ePulse = eSpace

  bJvel :: Behavior Double <-
    accumB 1 $ unions
      [ (\dt v -> v + 20 * realToFrac dt) <$> eTick
      , const (-20) <$ ePulse
      ]

  bJ :: Behavior Double <-
    accumB 5
      ((\v dt p -> p + v * realToFrac dt) <$> bJvel <@> eTick)

  let
    bCells :: Behavior Cells
    bCells =
      render <$> bJ

  let
    bScene :: Behavior Scene
    bScene =
      Scene
        <$> bCells
        <*> pure NoCursor

  pure (bScene, eDone)

render :: Double -> Cells
render j =
  set 5 (round j) (Cell 'J' blue mempty)
