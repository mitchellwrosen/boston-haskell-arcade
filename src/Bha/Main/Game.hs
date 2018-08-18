{-# LANGUAGE LambdaCase, NoImplicitPrelude, RecursiveDo, ScopedTypeVariables #-}

module Bha.Main.Game
  ( Game(..)
  , momentGame
  ) where

import Reactive.Banana.Frameworks (MomentIO)
import System.Random (mkStdGen)

import Bha.Banana.Prelude
import Bha.Banana.Prelude.Internal (Banana(..))
import Bha.Banana.Tick             (TickControl(TickSetDelta, TickTeardown),
                                    momentTick)
import Bha.Elm.Prelude             (ElmGame(..))

data Game
  = GameElm ElmGame
  | GameBanana (Events TermEvent -> Banana (Behavior Scene, Events ()))

momentGame
  :: Events TermEvent
  -> Game
  -> MomentIO (Behavior Scene, Events ())
momentGame eEvent = \case
  GameElm game ->
    momentElmGame eEvent game
  GameBanana game ->
    unBanana (game eEvent)

momentElmGame
  :: Events TermEvent
  -> ElmGame
  -> MomentIO (Behavior Scene, Events ())
momentElmGame eEvent (ElmGame init update view tickEvery) = mdo
  let
    init' = init (mkStdGen 0)

  eTick :: Events NominalDiffTime <-
    unBanana (momentTick (tickEvery init') eTickControl)

  let
    eTickControl :: Events TickControl
    eTickControl =
      filterJust
        ((\old -> \case
          Nothing ->
            Just TickTeardown
          Just model -> do
            let new = tickEvery model
            guard (new /= old)
            pure (TickSetDelta new))
        <$> bTickEvery <@> eModel)

  bTickEvery :: Behavior (Maybe NominalDiffTime) <-
    stepper (tickEvery init') (tickEvery <$> eModel')

  let
    eDone :: Events ()
    eDone =
      () <$ filterE isNothing eModel

  eModel :: Events (Maybe a) <-
    accumE (Just init')
      (unionWith const
        (((=<<) . update) .  Left <$> eTick)
        (((=<<) . update) . Right <$> eEvent))

  let
    -- eModel' :: Events a
    eModel' =
      filterJust eModel

  let
    eScene :: Events Scene
    eScene =
      view <$> eModel'

  bScene :: Behavior Scene <-
    stepper (view init') eScene

  pure (bScene, eDone)
