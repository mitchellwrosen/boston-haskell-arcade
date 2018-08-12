{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings, RecursiveDo,
             ScopedTypeVariables #-}

module Bha.Main
  ( main
  ) where

import Reactive.Banana.Bha
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
  -- Partition terminal events into two: those intended for the menu, and those
  -- intended for the game. How do we tell them apart? When there's an active
  -- game, it gets all of the input.
  let
    eEventForMenu = whenE (isNothing <$> bGame) eEvent :: Event Tb.Event
    eEventForGame = whenE (isJust    <$> bGame) eEvent :: Event Tb.Event

  -- Create the menu.
  (bMenuScene, eMenuOutput) :: (Behavior Scene, Event MainMenuOutput) <-
    momentMainMenu gamelist eEventForMenu

  -- Partition the menu's output into two: "I'm done" (escape) and "play this
  -- game" (enter).
  let
    eMenuDone = previewE ᴍainMenuOutputDone eMenuOutput :: Event ()
    eMenuGame = previewE ᴍainMenuOutputGame eMenuOutput :: Event Game

  (ebGameScene, eeGameDone) :: (Event (Behavior Scene), Event (Event ())) <-
    unpairE <$> execute (momentGame eEventForGame <$> eMenuGame)

  -- Event that fires when the current game ends.
  eGameDone :: Event () <-
    switchE eeGameDone

  -- The game currently being played.
  bGame :: Behavior (Maybe Game) <-
    stepper Nothing
      (unionWith const
        (Just    <$> eMenuGame)  -- When a game begins, step to it.
        (Nothing <$  eGameDone)) -- When the current game ends, step to Nothing.

  -- The scene to render.
  bScene :: Behavior Scene <-
    switchB
      -- Start by rendering the menu.
      bMenuScene
      (unionWith const
        -- When a new game starts, switch to it.
        ebGameScene
        -- When the current game ends, switch back to the menu.
        (bMenuScene <$ eGameDone))

  pure (bScene, eMenuDone)

-- | The master game list.
gamelist :: [([Char], Game)]
gamelist =
  [ ("Elm Example 1", Bha.Game.Impl.ElmExample.game)
  , ("Broken Game 2", GameFRP)
  ]
