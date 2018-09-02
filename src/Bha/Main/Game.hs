{-# LANGUAGE DataKinds, ExistentialQuantification, GADTs, KindSignatures,
             LambdaCase, NoImplicitPrelude, RecursiveDo,
             ScopedTypeVariables #-}

module Bha.Main.Game
  ( Game(..)
  , gameName
  , momentGame
  ) where

import Control.Monad.State        (StateT, execStateT)
import Data.Serialize             (Serialize)
import Reactive.Banana.Frameworks (MomentIO)
import System.Random              (mkStdGen, randomIO)

import qualified Data.Serialize as Serialize

import Bha.Banana.Prelude
import Bha.Banana.Prelude.Internal (Banana(..))
import Bha.Banana.Tick             (TickControl(TickSetDelta, TickTeardown),
                                    momentTick)
import Bha.Elm.Prelude             (ElmGame(..))
import Bha.Elm.Prelude.Internal    (Seed(..))
import Bha.Game                    (GameOutput(..))

data Game :: Type where
  GameElm
    :: [Char]
    -> ElmGame a
    -> Game

  GameBanana
    :: Serialize a
    => [Char]
    -> (Maybe a
        -> Events TermEvent
        -> Banana (Behavior Scene, Events (GameOutput a)))
    -> Game

gameName :: Game -> [Char]
gameName = \case
  GameElm    name _ -> name
  GameBanana name _ -> name

momentGame
  :: Maybe ByteString
  -> Events TermEvent
  -> Game
  -> MomentIO (Behavior Scene, Events (GameOutput ByteString))
momentGame save eEvent = \case
  GameElm _ game ->
    momentElmGame eEvent game

  GameBanana _ game ->
    let
      save' =
        save >>= either (const Nothing) Just . Serialize.decode
    in
      over (mapped._2.mapped.mapped) Serialize.encode
        (unBanana (game save' eEvent))

momentElmGame
  :: forall model.
     Events TermEvent
  -> ElmGame model
  -> MomentIO (Behavior Scene, Events (GameOutput ByteString))
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
    eDone :: Events (GameOutput ByteString)
    eDone =
      GameOver Nothing <$ filterE isNothing eModel

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
