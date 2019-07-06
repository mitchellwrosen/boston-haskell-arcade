-- | Example FRP-style game.

module Bha.Game.Impl.BananaExample
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
  eTick :: Events Seconds <-
    momentTick (Just 1) (TickTeardown <$ eDone)

  bElapsed :: Behavior Seconds <-
    accumB 0 ((+) <$> eTick)

  let
    eDone :: Events ()
    eDone =
      leftmostE
        [ () <$ filterE (== KeyEsc) eKey
        , () <$ filterE (> 10) eCount
        ]

  eCount :: Events Int <-
    accumE 0 ((+1) <$ eKey)

  bCount :: Behavior Int <-
    stepper 0 eCount

  let
    bCells :: Behavior Cells
    bCells =
      f <$> bCount <*> bElapsed
     where
      f :: Int -> Seconds -> Cells
      f n elapsed =
        mconcat
          [ text 0 0 mempty mempty "I am a banana game!"
          , text 0 2 mempty mempty "Let's count to 10."
          , text 2 4 mempty mempty (show n)
          , text 0 6 mempty mempty ("Elapsed time: " ++ show elapsed)
          ]
  let
    bScene :: Behavior Scene
    bScene =
      Scene
        <$> bCells
        <*> pure NoCursor

  pure (bScene, pure mempty, never, eDone)
