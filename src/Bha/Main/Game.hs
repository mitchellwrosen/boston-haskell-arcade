{-# LANGUAGE LambdaCase, NamedFieldPuns, NoImplicitPrelude, RecursiveDo,
             ScopedTypeVariables #-}

module Bha.Main.Game
  ( momentGame
  ) where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Termbox.Banana             (Scene(..))

import qualified Termbox.Banana as Tb

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
  (eTick, fireTick) :: (Event NominalDiffTime, NominalDiffTime -> IO ()) <-
    newEvent

  -- Two mutable cells to communicate with the background ticking thread: how
  -- often to tick, and whether or not the game is done.
  tickEveryVar :: TVar (Maybe NominalDiffTime) <-
    liftIO (newTVarIO (tickEvery init))
  doneVar :: TMVar () <-
    liftIO (if isDone init then newTMVarIO () else newEmptyTMVarIO)

  -- Spawn the background ticking thread, which gracefully ends itself when
  -- the game is over.
  (liftIO . void . forkIO . fix) $ \loop ->
    let
      act1 :: STM (IO ())
      act1 = do
        readTMVar doneVar
        pure (pure ())

      act2 :: STM (IO ())
      act2 =
        readTVar tickEveryVar >>= \case
          Nothing ->
            empty
          Just sleep ->
            pure $ do
              threadDelay (floor ((realToFrac sleep :: Double) * 1000000))
              fireTick sleep
              loop
    in
      join (atomically (act1 <|> act2))

  let
    eDone :: Event ()
    eDone =
      filterJust ((guard . isDone) <$> eModel)

  eModel :: Event a <-
    accumE init
      (unionWith const
        (update . Left  <$> eTick)
        (update . Right <$> eEvent))

  -- Communicate with the tick thread.
  reactimate
    ((\model ->
      if isDone model
        then atomically (void (tryPutTMVar doneVar ()))
        else atomically (writeTVar tickEveryVar (tickEvery model)))
    <$> eModel)

  let
    eScene :: Event Scene
    eScene =
      view <$> eModel

  bScene :: Behavior Scene <-
    stepper (view init) eScene

  pure (bScene, eDone)
