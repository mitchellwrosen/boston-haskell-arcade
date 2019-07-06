module Bha.Game.Impl.FlappingJ
  ( moment
  ) where

import Bha.Banana.Prelude
import Bha.Banana.Tick


moment
  :: Int
  -> Int
  -> Events (Text, Void)
  -> Events Key
  -> Banana
       ( Behavior Scene
       , Behavior (HashSet Text)
       , Events (Text, Void)
       , Events ()
       )
moment _ _ _ eKey = mdo
  let
    eEsc   = filterE (== KeyEsc) eKey
    eSpace = filterE (== KeySpace) eKey

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
