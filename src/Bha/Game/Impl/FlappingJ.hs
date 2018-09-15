module Bha.Game.Impl.FlappingJ
  ( moment
  ) where

import Bha.Banana.Prelude
import Bha.Banana.Tick

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
  let
    eEsc   = filterE (== EventKey KeyEsc   False) eEvent
    eSpace = filterE (== EventKey KeySpace False) eEvent

    eDone = () <$ eEsc

  eTick :: Events Seconds <-
    momentTick (Just (1/30)) (TickTeardown <$ eDone)

  let
    ePulse = eSpace

  bJvel :: Behavior Double <-
    accumB 1 $ unions
      [ (\dt v -> v + 80 * realToFrac dt) <$> eTick
      , const (-40) <$ ePulse
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

  pure (bScene, pure mempty, never, eDone)

render :: Double -> Cells
render j =
  set 5 (round j) (Cell 'J' blue mempty)
