{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings, RecursiveDo,
             ScopedTypeVariables #-}

module Bha.Main
  ( main
  ) where

import Reactive.Banana.Bha
import Reactive.Banana.Frameworks (MomentIO, execute, reactimate)
import System.Directory           (XdgDirectory(..), createDirectoryIfMissing,
                                   getXdgDirectory)
import System.FilePath            ((</>))
import Termbox.Banana             (InputMode(..), MouseMode(..), OutputMode(..))

import qualified Data.ByteString       as ByteString
import qualified Termbox.Banana        as Tb

import Bha.Banana.Prelude
import Bha.Game           (ɢameOver)
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

gamelist :: [Game]
gamelist =
  [ GameElm    "Snake"            Bha.Game.Impl.Snake.game
  , GameBanana "2048"             Bha.Game.Impl.H2048.moment
  , GameElm    "Grain Main"       Bha.Game.Impl.GrainMan.game
  , GameElm    "Elm Example 1"    Bha.Game.Impl.ElmExample.game
  , GameBanana "Banana Example 1" Bha.Game.Impl.BananaExample.moment
  ]

------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

main :: IO ()
main = do
  dir <- getXdgDirectory XdgData "boston-haskell-arcade"
  createDirectoryIfMissing True dir
  Tb.main (InputModeEsc MouseModeYes) OutputModeNormal (main' dir)

main'
  :: FilePath
  -> Events TermEvent
  -> Behavior (Int, Int)
  -> MomentIO (Behavior Scene, Events ())
main' dir eEvent _bSize = mdo
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

  (ebGameScene, eeGameOutput) :: (Events (Behavior Scene), Events (Events (GameOutput ByteString))) <-
    let
      f :: Game -> MomentIO (Behavior Scene, Events (GameOutput ByteString))
      f game = do
        save :: Maybe ByteString <-
          liftIO
            ((Just <$> ByteString.readFile (dir </> gameName game)) <|>
              pure Nothing)
        momentGame save eEventForGame game
    in
      unpairE <$> execute (f <$> eMenuGame)

  -- Event that fires when the current game ends.
  eGameDone :: Events (Maybe ByteString) <-
    previewE ɢameOver <$> switchE eeGameOutput

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

  reactimate $
    let
      f :: Maybe Game -> Maybe ByteString -> IO ()
      f (Just game) (Just save) = do
        ByteString.writeFile (dir </> gameName game) save
      f _ _ =
        pure ()
    in
      f <$> bGame <@> eGameDone

  pure (bScene, eMenuDone)
