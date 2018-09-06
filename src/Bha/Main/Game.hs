module Bha.Main.Game
  ( Game(..)
  , gameName
  , momentGame
  ) where

import Control.Monad.Except
import Reactive.Banana.Frameworks (MomentIO, execute)
import System.Directory           (createDirectoryIfMissing)
import System.FilePath            ((</>))
import System.Random              (randomIO, randomRIO)

import qualified Data.ByteString as ByteString
import qualified Data.Text       as Text

import Bha.Banana.Prelude
import Bha.Banana.Prelude.Internal (Banana(..))
import Bha.Banana.Tick             (TickControl(TickSetDelta, TickTeardown),
                                    momentTick)
import Bha.Elm.Prelude             (ElmGame(..))
import Bha.Elm.Prelude.Internal    (ElmF(..), runInit, runUpdate)

data Game :: Type where
  GameElm
    :: [Char]
    -> ElmGame a
    -> Game

  GameBanana
    :: [Char]
    -> (Events TermEvent -> Banana (Behavior Scene, Events ()))
    -> Game

gameName :: Game -> [Char]
gameName = \case
  GameElm    name _ -> name
  GameBanana name _ -> name

momentGame
  :: Events TermEvent
  -> Game
  -> MomentIO (Behavior Scene, Events ())
momentGame eEvent = \case
  GameElm name game ->
    momentElmGame name eEvent game

  GameBanana _ game ->
    unBanana (game eEvent)

momentElmGame
  :: forall model.
     [Char]
  -> Events TermEvent
  -> ElmGame model
  -> MomentIO (Behavior Scene, Events ())
momentElmGame name eEvent (ElmGame init update view tickEvery) = mdo
  model0 :: model <-
    runInit (interpretElmIO name) init

  let
    tickEvery0 :: Maybe NominalDiffTime
    tickEvery0 =
      tickEvery model0

  eUpdate :: Events (Maybe ((), model)) <-
    let
      f model event =
        runUpdate model (interpretElmIO name) (update event)

      eInput :: Events (Either NominalDiffTime TermEvent)
      eInput =
        leftmostE
          [ Left <$> eTick
          , Right <$> eEvent
          ]
    in
      execute (f <$> bModel <@> eInput)

  let
    eModel :: Events model
    eModel =
      (\((), model) -> model) <$> filterJust eUpdate

  bModel :: Behavior model <-
    stepper model0 eModel

  eTick :: Events NominalDiffTime <-
    unBanana (momentTick tickEvery0 eTickControl)

  let
    eTickControl :: Events TickControl
    eTickControl =
      filterJust
        ((\old -> \case
          Nothing ->
            Just TickTeardown

          Just ((), model) -> do
            let new = tickEvery model
            guard (new /= old)
            pure (TickSetDelta new))
        <$> bTickEvery <@> eUpdate)

  bTickEvery :: Behavior (Maybe NominalDiffTime) <-
    stepper tickEvery0 (tickEvery <$> eModel)

  let
    eDone :: Events ()
    eDone =
      () <$ filterE isNothing eUpdate

  let
    eScene :: Events Scene
    eScene =
      view <$> eModel

  bScene :: Behavior Scene <-
    stepper (view model0) eScene

  pure (bScene, eDone)

interpretElmIO :: MonadIO m => [Char] -> ElmF (m x) -> m x
interpretElmIO name = \case
  Save key value k -> do
    let dir = bhaDataDir </> name
    let file = dir </> Text.unpack key
    liftIO $ do
      createDirectoryIfMissing True dir
      ByteString.writeFile file value
    k

  Load key k -> do
    let file = bhaDataDir </> name </> Text.unpack key
    value :: Maybe ByteString <- liftIO $
      asum
        [ Just <$> ByteString.readFile file
        , pure Nothing
        ]
    k value

  RandomInt lo hi k ->
    liftIO (randomRIO (lo, hi)) >>= k

  RandomPct k ->
    liftIO randomIO >>= k
