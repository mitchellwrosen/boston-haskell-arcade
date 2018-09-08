module Bha.Banana.Tick
  ( TickControl(..)
  , momentTick
  ) where

import Reactive.Banana.Frameworks

import Bha.Banana.Prelude          hiding (reactimate)
import Internal.Bha.Banana.Prelude (Banana(..))

data TickControl
  = TickSetDelta (Maybe NominalDiffTime)
    -- ^ Update the rate at which ticks occur.
  | TickTeardown
    -- ^ Stop ticking forever; tears down the tick thread.

-- | Create a dynamic tick event that carries the delta since the last tick.
-- Tick speed is dynamic, and can be "frozen".
momentTick
  :: Maybe NominalDiffTime
     -- ^ Initial tick rate.
  -> Events TickControl
     -- ^ Updates to the tick thread behavior.
  -> Banana (Events NominalDiffTime)
momentTick delta0 eControl = Banana $ do
  (eTick, fireTick) :: (Events NominalDiffTime, NominalDiffTime -> IO ()) <-
    newEvent

  -- Two mutable cells to communicate with the background ticking thread: how
  -- often to tick, and whether or not the game is done.
  tickDeltaVar :: TVar (Maybe NominalDiffTime) <-
    liftIO (newTVarIO delta0)
  doneVar :: TMVar () <-
    liftIO newEmptyTMVarIO

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
        readTVar tickDeltaVar >>= \case
          Nothing ->
            empty
          Just sleep ->
            pure $ do
              threadDelay (floor ((realToFrac sleep :: Double) * 1000000))
              fireTick sleep
              loop
    in
      join (atomically (act1 <|> act2))

  reactimate (handleControl tickDeltaVar doneVar <$> eControl)

  pure eTick

handleControl
  :: TVar (Maybe NominalDiffTime)
  -> TMVar ()
  -> TickControl
  -> IO ()
handleControl tickDeltaVar doneVar = \case
  TickSetDelta delta ->
    atomically (writeTVar tickDeltaVar delta)
  TickTeardown ->
    atomically (void (tryPutTMVar doneVar ()))
