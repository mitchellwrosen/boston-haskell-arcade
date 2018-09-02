-- | Example FRP-style game.

{-# LANGUAGE NoImplicitPrelude, RecursiveDo, ScopedTypeVariables #-}

module Bha.Game.Impl.BananaExample
  ( moment
  ) where

import Bha.Banana.Prelude
import Bha.Banana.Tick

moment
  :: Maybe ()
  -> Events TermEvent
  -> Banana (Behavior Scene, Events (GameOutput ()))
moment _ eEvent = mdo
  eTick :: Events NominalDiffTime <-
    momentTick (Just 1) (TickTeardown <$ eDone)

  bElapsed :: Behavior NominalDiffTime <-
    accumB 0 ((+) <$> eTick)

  let
    eDone :: Events (GameOutput ())
    eDone =
      unionWith const
        (GameOver Nothing <$ filterE (== EventKey KeyEsc False) eEvent)
        (GameOver Nothing <$ filterE (> 10) eCount)

  eCount :: Events Int <-
    accumE 0 ((+1) <$ eEvent)

  bCount :: Behavior Int <-
    stepper 0 eCount

  let
    bCells :: Behavior Cells
    bCells =
      f <$> bCount <*> bElapsed
     where
      f :: Int -> NominalDiffTime -> Cells
      f n elapsed =
        mconcat
          [ tbstr 0 0 mempty mempty "I am a banana game!"
          , tbstr 0 2 mempty mempty "Let's count to 10."
          , tbstr 2 4 mempty mempty (show n)
          , tbstr 0 6 mempty mempty ("Elapsed time: " ++ show elapsed)
          ]
  let
    bScene :: Behavior Scene
    bScene =
      Scene
        <$> bCells
        <*> pure NoCursor

  pure (bScene, eDone)
