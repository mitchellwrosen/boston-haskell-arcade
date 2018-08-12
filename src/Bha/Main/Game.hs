{-# LANGUAGE LambdaCase, NamedFieldPuns, NoImplicitPrelude, RecursiveDo,
             ScopedTypeVariables #-}

module Bha.Main.Game
  ( momentGame
  ) where

import Reactive.Banana
import Termbox.Banana  (Scene(..))

import qualified Termbox.Banana as Tb

import Bha.Game
import Bha.Prelude

momentGame
  :: MonadMoment m
  => Event Tb.Event
  -> Game
  -> m (Behavior Scene, Event ())
momentGame eEvent = \case
  GameElm game ->
    momentElmGame eEvent game
  GameFRP ->
    undefined -- TODO GameFRP moment

momentElmGame
  :: MonadMoment m
  => Event Tb.Event
  -> ElmGame
  -> m (Behavior Scene, Event ())
momentElmGame eEvent ElmGame { init, update, view, isDone, tickEvery } = mdo
  let
    eDone :: Event ()
    eDone =
      filterJust ((guard . isDone) <$> eModel)

  let
    -- TODO momentElmGame: tick event
    eTick :: Event ()
    eTick =
      never

  eModel :: Event a <-
    accumE init
      (unionWith const
        (update . Left <$> eTick)
        (update . Right <$> eEvent))

  let
    eScene :: Event Scene
    eScene =
      view <$> eModel

  bScene :: Behavior Scene <-
    stepper (view init) eScene

  pure (bScene, eDone)
