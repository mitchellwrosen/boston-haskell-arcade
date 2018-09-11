module Bha.Main
  ( main
  ) where

import Control.Concurrent
import Control.Exception          (throwIO)
import Data.List.Split            (splitOn)
import Reactive.Banana.Frameworks (MomentIO, execute, newEvent, reactimate)
import System.Directory           (createDirectoryIfMissing)
import System.Environment         (getArgs)
import Termbox.Banana             (InputMode(..), MouseMode(..), OutputMode(..))
import Text.Read                  (readMaybe)

import qualified Data.Aeson         as Aeson
import qualified Network.WebSockets as WebSockets
import qualified Termbox.Banana     as Tb

import Bha.Banana.Prelude hiding (reactimate)
import Bha.Main.Game
import Bha.Main.Menu
import Internal.Bha.View  (sceneToTbScene)

import qualified Bha.Game.Impl.BananaExample
import qualified Bha.Game.Impl.BlimpBoy
import qualified Bha.Game.Impl.ElmExample
import qualified Bha.Game.Impl.FlappingJ
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
  , GameElm    "Blimp Boy"        Bha.Game.Impl.BlimpBoy.game
  , GameBanana "Flapping J"       Bha.Game.Impl.FlappingJ.moment
  , GameElm    "Grain Man"        Bha.Game.Impl.GrainMan.game
  , GameElm    "Elm Example 1"    Bha.Game.Impl.ElmExample.game
  , GameBanana "Banana Example 1" Bha.Game.Impl.BananaExample.moment
  ]

------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

main :: IO ()
main =
  getArgs >>= \case
    [splitOn ":" -> [host, readMaybe -> Just port]] ->
      WebSockets.runClient host port "/" (\conn -> main' (Just conn))

    _ ->
      main' Nothing

main' :: Maybe WebSockets.Connection -> IO ()
main' mconn = do
  createDirectoryIfMissing True bhaDataDir

  Tb.main (InputModeEsc MouseModeYes) OutputMode256 $ \eEvent bSize -> do
    eMessage :: Events ServerMessage <-
      case mconn of
        Nothing ->
          pure never

        Just conn -> do
          (eMessage, fireMessage) <-
            newEvent

          tid <- liftIO myThreadId
          let killMain _ = killThread tid

          (liftIO . void . (`forkFinally` killMain) . forever) $ do
            bytes <- WebSockets.receiveData conn
            case Aeson.eitherDecodeStrict' bytes of
              Left err ->
                throwIO (userError err)

              Right message ->
                fireMessage message

          pure eMessage

    let
      send :: ByteString -> IO ()
      send =
        case mconn of
          Nothing ->
            const (throwIO (userError "Not connected"))
          Just conn ->
            WebSockets.sendTextData conn

    main'' send eMessage eEvent bSize

main''
  :: (ByteString -> IO ())
  -> Events ServerMessage
  -> Events TermEvent
  -> Behavior (Int, Int)
  -> MomentIO (Behavior Tb.Scene, Events ())
main'' send eMessage eEvent _bSize = mdo

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

  (ebGameScene, eeGameOutput, eeGameDone)
      :: ( Events (Behavior Scene)
         , Events (Events ByteString)
         , Events (Events ())
         ) <- do
    let
      f :: Events (a, b, c) -> (Events a, Events b, Events c)
      f xs =
        ((^. _1) <$> xs, (^. _2) <$> xs, (^. _3) <$> xs)

    f <$> execute (momentGame eMessage eEventForGame <$> eMenuGame)

  -- Event that fires when the current game emits a payload for the server.
  eGameOutput :: Events ByteString <-
    switchE eeGameOutput

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

  reactimate (send <$> eGameOutput)

  pure (sceneToTbScene <$> bScene, eMenuDone)
