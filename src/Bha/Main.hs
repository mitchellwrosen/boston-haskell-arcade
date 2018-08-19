{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings, RecursiveDo,
             ScopedTypeVariables #-}

module Bha.Main
  ( main
  ) where

import Reactive.Banana.Bha
import Reactive.Banana.Frameworks (MomentIO, execute)
import Termbox.Banana             (InputMode(..), MouseMode(..), OutputMode(..))

import qualified Termbox.Banana as Tb

import Bha.Banana.Prelude
import Bha.Main.Game
import Bha.Main.Menu

import qualified Bha.Game.Impl.BananaExample
import qualified Bha.Game.Impl.ElmExample
import qualified Bha.Game.Impl.GrainMan
import qualified Bha.Game.Impl.H2048
import qualified Bha.Game.Impl.Snake

------------------------------------------------------------------------------
-- Game list
------------------------------------------------------------------------------

gamelist :: [([Char], Game)]
gamelist =
  [ ("Elm Example 1",    GameElm    Bha.Game.Impl.ElmExample.game)
  , ("Banana Example 1", GameBanana Bha.Game.Impl.BananaExample.moment)
  , ("2048",             GameBanana Bha.Game.Impl.H2048.moment)
  , ("Snake",            GameElm    Bha.Game.Impl.Snake.game)
  , ("Grain Man",        GameElm    Bha.Game.Impl.GrainMan.game)
  ]

------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

main :: IO ()
main =
  Tb.main (InputModeEsc MouseModeYes) OutputModeNormal main'

main'
  :: Events TermEvent
  -> Behavior (Int, Int)
  -> MomentIO (Behavior Scene, Events ())
main' eEvent _bSize = mdo
  -- Partition terminal events into two: those intended for the menu, and those
  -- intended for the game. How do we tell them apart? When there's an active
  -- game, it gets all of the input.
  let
    eEventForMenu = whenE (isNothing <$> bGame) eEvent :: Events TermEvent
    eEventForGame = whenE (isJust    <$> bGame) eEvent :: Events TermEvent

  -- Create the menu.
  (bMenuScene, eMenuOutput) :: (Behavior Scene, Events MainMenuOutput) <-
    momentMainMenu gamelist eEventForMenu

  -- Partition the menu's output into two: "I'm done" (escape) and "play this
  -- game" (enter).
  let
    eMenuDone = previewE ᴍainMenuOutputDone eMenuOutput :: Events ()
    eMenuGame = previewE ᴍainMenuOutputGame eMenuOutput :: Events Game

  (ebGameScene, eeGameDone) :: (Events (Behavior Scene), Events (Events ())) <-
    unpairE <$> execute (momentGame eEventForGame <$> eMenuGame)

  -- Event that fires when the current game ends.
  eGameDone :: Events () <-
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
