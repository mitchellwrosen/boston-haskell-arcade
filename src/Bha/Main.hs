module Bha.Main
  ( main
  ) where

import Bha.Banana.Prelude
import Bha.Main.Game
import Bha.Main.Menu
import Internal.Bha.View  (sceneToTbScene)

import qualified Bha.Game.Impl.BananaExample
import qualified Bha.Game.Impl.BlimpBoy
import qualified Bha.Game.Impl.ElmExample
import qualified Bha.Game.Impl.FlappingJ
import qualified Bha.Game.Impl.GrainMan
import qualified Bha.Game.Impl.H2048
import qualified Bha.Game.Impl.LambdaChat
import qualified Bha.Game.Impl.Paint
import qualified Bha.Game.Impl.Snake

import Data.Aeson                 ((.=))
import Data.List.Split            (splitOn)
import Reactive.Banana.Frameworks (Future, MomentIO, changes, execute, newEvent,
                                   reactimate')
import System.Directory           (createDirectoryIfMissing)
import System.Environment         (getArgs)
import System.IO.Error            (userError)
import Termbox.Banana             (InputMode(..), MouseMode(..), OutputMode(..))
import Text.Read                  (readMaybe)

import qualified Data.Aeson           as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.HashSet         as HashSet
import qualified Network.WebSockets   as WebSockets
import qualified SlaveThread
import qualified Termbox.Banana       as Termbox


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
  , GameElm    "LambdaChat"       Bha.Game.Impl.LambdaChat.game
  , GameElm    "Elm Example 1"    Bha.Game.Impl.ElmExample.game
  , GameBanana "Banana Example 1" Bha.Game.Impl.BananaExample.moment
  ]

------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

main :: IO ()
main =
  getArgs >>= \case
    [splitOn ":" -> [host, readMaybe -> Just port]] -> do
      WebSockets.runClient host port "" (\conn -> main' (Just conn))

    _ ->
      main' Nothing

main'
  :: Maybe WebSockets.Connection
  -> IO ()
main' mconn = do
  createDirectoryIfMissing True bhaDataDir

  Termbox.run_ (InputModeEsc MouseModeYes) OutputMode256 $ \eEvent bSize -> do
    eMessage :: Events ServerMessage <-
      case mconn of
        Nothing ->
          pure never

        Just conn -> do
          (eMessage, fireMessage) <-
            newEvent

          (void . liftIO . SlaveThread.fork . forever) $ do
            bytes :: ByteString <-
              WebSockets.receiveData conn

            case Aeson.eitherDecodeStrict' bytes of
              Left err ->
                throwIO (userError (show (err, bytes)))

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
  -> Events Termbox.Event
  -> Behavior (Int, Int)
  -> MomentIO (Behavior Termbox.Scene, Events ())
main'' send eMessage eEvent bSize = mdo
  -- Create the menu.
  (bMenuScene, eMenuOutput) :: (Behavior Scene, Events MainMenuOutput) <-
    momentMainMenu gamelist (whenE (is _Nothing <$> bGame) eEvent) bSize

  -- Partition the menu's output into two: "I'm done" (escape) and "play this
  -- game" (enter).
  let
    eMenuDone = previewE ᴍainMenuOutputDone eMenuOutput :: Events ()
    eMenuGame = previewE ᴍainMenuOutputGame eMenuOutput :: Events Game

  (ebGameScene, ebGameSubscribe, eeGameDone)
      :: ( Events (Behavior Scene)
         , Events (Behavior (HashSet Text))
         , Events (Events ())
         ) <- do
    let
      doRunGame
        :: Game
        -> MomentIO
             ( Behavior Scene
             , Behavior (HashSet Text)
             , Events ()
             )
      doRunGame game = mdo
        (width, height) <-
          valueB bSize

        bGate :: Behavior Bool <-
          stepper True (False <$ eThisGameDone)

        (bThisGameScene, bThisGameSubscribe, eThisGameDone) <-
          momentGame send width height eMessage (whenE bGate eEvent) game

        pure (bThisGameScene, bThisGameSubscribe, eThisGameDone)

      f :: Events (a, b, c) -> (Events a, Events b, Events c)
      f xs =
        ((^. _1) <$> xs, (^. _2) <$> xs, (^. _3) <$> xs)

    f <$> execute (doRunGame <$> eMenuGame)

  -- Event that fires when the current game ends.
  eGameDone :: Events () <-
    switchE eeGameDone

  -- The game currently being played.
  bGame :: Behavior (Maybe Game) <-
    stepper Nothing
      (leftmostE
        [ Just    <$> eMenuGame -- When a game begins, step to it.
        , Nothing <$  eGameDone -- When the current game ends, step to Nothing.
        ])

  -- The scene to render.
  bScene :: Behavior Scene <-
    switchB
      -- Start by rendering the menu.
      bMenuScene
      (leftmostE
        [ --When a new game starts, switch to it.
          ebGameScene

          -- When the current game ends, switch back to the menu.
        , bMenuScene <$ eGameDone
        ])

  manageSubscriptions send ebGameSubscribe eGameDone

  pure (sceneToTbScene <$> bScene, eMenuDone)


manageSubscriptions
  :: (ByteString -> IO ())
  -> Events (Behavior (HashSet Text))
  -> Events ()
  -> MomentIO ()
manageSubscriptions send ebGameSubscribe eGameDone = do
  bSubscribe :: Behavior (HashSet Text) <-
    switchB
      (pure mempty)
      (leftmostE
        [ ebGameSubscribe
        , (pure mempty <$ eGameDone)
        ])
  eFutureSubscribe :: Events (Future (HashSet Text)) <-
    changes bSubscribe
  reactimate' (resubscribe send <$> bSubscribe <@> eFutureSubscribe)


resubscribe
  :: (ByteString -> IO ())
  -> HashSet Text
  -> Future (HashSet Text)
  -> Future (IO ())
resubscribe send old =
  fmap f
 where
  f :: HashSet Text -> IO ()
  f new = do
    let
      toSubscribe :: HashSet Text
      toSubscribe =
        HashSet.difference new old

    let
      toUnsubscribe :: HashSet Text
      toUnsubscribe =
        HashSet.difference old new

    unless (null toUnsubscribe) $
      bloop
        [ "type"   .= ("unsubscribe" :: Text)
        , "topics" .= HashSet.difference old new
        ]

    unless (null toSubscribe) $ do
      bloop
        [ "type"   .= ("subscribe" :: Text)
        , "topics" .= HashSet.difference new old
        ]

  bloop :: [(Text, Aeson.Value)] -> IO ()
  bloop =
    send . ByteString.Lazy.toStrict . Aeson.encode . Aeson.object
