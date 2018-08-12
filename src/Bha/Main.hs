{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings, RecursiveDo,
             ScopedTypeVariables #-}

module Bha.Main
  ( main
  ) where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Termbox.Banana             (InputMode(..), MouseMode(..), OutputMode(..),
                                   Scene(..))

import qualified Termbox.Banana as Tb

import Bha.Game
import Bha.Main.Game
import Bha.Main.Menu
import Bha.Prelude

import qualified Bha.Game.Impl.ElmExample

main :: IO ()
main =
  Tb.main (InputModeEsc MouseModeYes) OutputModeNormal main'

main'
  :: Event Tb.Event
  -> Behavior (Int, Int)
  -> MomentIO (Behavior Scene, Event ())
main' eEvent _bSize = mdo
  (bMenuScene, eMenuOutput) <-
    momentMainMenu gamelist (whenE (isNothing <$> bGame) eEvent)

  let
    eMenuDone :: Event ()
    eMenuDone =
      filterJust (f <$> eMenuOutput)
     where
      f :: MainMenuOutput -> Maybe ()
      f = \case
        MainMenuOutputDone -> Just ()
        MainMenuOutputGame{} -> Nothing

  -- Event that fires whenever a new game is started.
  -- TODO previewE
  let
    eGame :: Event Game
    eGame =
      filterJust (f <$> eMenuOutput)
     where
      f :: MainMenuOutput -> Maybe Game
      f = \case
        MainMenuOutputGame game -> Just game
        MainMenuOutputDone      -> Nothing

  -- TODO clean this gameGuts stuff up
  let
    eGameGuts :: Event (Behavior Scene, Event ())
    eGameGuts =
      observeE (momentGame (whenE (isJust <$> bGame) eEvent) <$> eGame)

  eGameDone :: Event () <-
    switchE (snd <$> eGameGuts)

  -- The game currently being played.
  bGame :: Behavior (Maybe Game) <-
    stepper Nothing
      (unionWith const
        (Just    <$> eGame)
        (Nothing <$  eGameDone))

  bScene :: Behavior Scene <-
    switchB bMenuScene
      (unionWith const
        (bMenuScene <$ eGameDone)
        (fst <$> eGameGuts))

  pure (bScene, eMenuDone)

-- | The master game list.
gamelist :: [([Char], Game)]
gamelist =
  [ ("Elm Example 1", Bha.Game.Impl.ElmExample.game)
  , ("Broken Game 2", GameFRP)
  ]
