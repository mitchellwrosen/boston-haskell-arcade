{-# LANGUAGE ExistentialQuantification, LambdaCase, NoImplicitPrelude,
             RecursiveDo, ScopedTypeVariables #-}

module Bha.Main.Game
  ( Game(..)
  , momentGame
  ) where

import Control.Monad.State        (StateT, execStateT)
import Reactive.Banana.Frameworks (MomentIO)
import System.Random              (mkStdGen, randomIO)

import Bha.Banana.Prelude
import Bha.Banana.Prelude.Internal (Banana(..))
import Bha.Banana.Tick             (TickControl(TickSetDelta, TickTeardown),
                                    momentTick)
import Bha.Elm.Prelude             (ElmGame(..))
import Bha.Elm.Prelude.Internal    (Seed(..))

data Game
  = forall model. GameElm (ElmGame model)
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
  :: forall model.
     Events TermEvent
  -> ElmGame model
  -> MomentIO (Behavior Scene, Events ())
momentElmGame eEvent (ElmGame init update view tickEvery) = mdo
  seed :: Seed <-
    liftIO (Seed . mkStdGen <$> randomIO)

  let
    init' = init seed

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
        (stepElm . update .  Left <$> eTick)
        (stepElm . update . Right <$> eEvent))

  let
    eModel' :: Events model
    eModel' =
      filterJust eModel

  let
    eScene :: Events Scene
    eScene =
      view <$> eModel'

  bScene :: Behavior Scene <-
    stepper (view init') eScene

  pure (bScene, eDone)

stepElm :: StateT model Maybe () -> Maybe model -> Maybe model
stepElm m =
  (>>= execStateT m)
