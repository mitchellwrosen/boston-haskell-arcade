module Bha.Main
  ( main
  ) where

import Reactive.Banana.Bha
import Reactive.Banana.Frameworks (MomentIO, execute)
import System.Directory           (createDirectoryIfMissing)
import Termbox.Banana             (InputMode(..), MouseMode(..), OutputMode(..))

import qualified Termbox.Banana as Tb

import Bha.Banana.Prelude
import Bha.Main.Game
import Bha.Main.Menu

import qualified Bha.Game.Impl.BananaExample
import qualified Bha.Game.Impl.ElmExample
import qualified Bha.Game.Impl.GrainMan
import qualified Bha.Game.Impl.H2048
import qualified Bha.Game.Impl.Paint
import qualified Bha.Game.Impl.Snake

------------------------------------------------------------------------------
-- Game list
------------------------------------------------------------------------------

gamelist :: [Game]
gamelist =
  [ GameElm    "Snake"            Bha.Game.Impl.Snake.game
  , GameBanana "2048"             Bha.Game.Impl.H2048.moment
  , GameBanana "Paint"            Bha.Game.Impl.Paint.moment
  , GameElm    "Grain Man"        Bha.Game.Impl.GrainMan.game
  , GameElm    "Elm Example 1"    Bha.Game.Impl.ElmExample.game
  , GameBanana "Banana Example 1" Bha.Game.Impl.BananaExample.moment
  ]

------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

main :: IO ()
main = do
  createDirectoryIfMissing True bhaDataDir
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

  (ebGameScene, eeGameOutput) :: (Events (Behavior Scene), Events (Events ())) <-
    unpairE <$> execute (momentGame eEventForGame <$> eMenuGame)

  -- Event that fires when the current game ends.
  eGameDone :: Events () <-
    switchE eeGameOutput

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
