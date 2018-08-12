{-# LANGUAGE LambdaCase, NamedFieldPuns, NoImplicitPrelude, RecursiveDo,
             ScopedTypeVariables, ViewPatterns #-}

module Bha.Main.Game
  ( momentGame
  ) where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Termbox.Banana             (Scene(..))

import qualified Termbox.Banana as Tb

import Bha.Frp.Tick (TickControl(TickSetDelta, TickTeardown), momentTick)
import Bha.Game
import Bha.Prelude

momentGame
  :: Event Tb.Event
  -> Game
  -> MomentIO (Behavior Scene, Event ())
momentGame eEvent = \case
  GameElm game ->
    momentElmGame eEvent game
  GameFRP ->
    undefined -- TODO GameFRP moment

momentElmGame
  :: Event Tb.Event
  -> ElmGame
  -> MomentIO (Behavior Scene, Event ())
momentElmGame eEvent ElmGame { init, update, view, isDone, tickEvery } = mdo
  eTick :: Event NominalDiffTime <-
    momentTick (tickEvery init) eTickControl

  let
    eTickControl :: Event TickControl
    eTickControl =
      filterJust
        ((\old model ->
          if isDone model
            then
              Just TickTeardown
            else do
              let new = tickEvery model
              guard (new /= old)
              pure (TickSetDelta new))
        <$> bTickEvery <@> eModel)

  bTickEvery :: Behavior (Maybe NominalDiffTime) <-
    stepper (tickEvery init) (tickEvery <$> eModel)

  let
    eDone :: Event ()
    eDone =
      filterJust ((guard . isDone) <$> eModel)

  eModel :: Event a <-
    accumE init
      (unionWith const
        (update . Left  <$> eTick)
        (update . Right <$> eEvent))

  let
    eScene :: Event Scene
    eScene =
      view <$> eModel

  bScene :: Behavior Scene <-
    stepper (view init) eScene

  pure (bScene, eDone)
