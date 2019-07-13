module Bha.Main.Game
  ( Game(..)
  , gameName
  , ServerMessage(..)
  , momentGame
  ) where

import Bha.Banana.Prelude
import Bha.Banana.Tick             (TickControl(TickSetDelta, TickTeardown),
                                    momentTick)
import Bha.Elm.Prelude             (ElmGame(..), Input(..))
import Bha.Internal.Banana.Prelude (Banana, runBanana)
import Bha.Internal.Elm.Prelude    (ElmF(..), runInit, runUpdate)

import Data.Aeson                 (FromJSON(..), ToJSON, Value, (.:), (.=))
import Data.Tuple                 (uncurry)
import Reactive.Banana.Frameworks (MomentIO, execute, reactimate)
import System.Directory           (createDirectoryIfMissing)
import System.FilePath            ((</>))
import System.IO.Error            (userError)
import System.Random              (randomIO, randomRIO)

import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.Types     as Aeson
import qualified Data.ByteString      as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.Text            as Text
import qualified Termbox.Banana       as Termbox


data Game :: Type where
  GameElm
    :: (FromJSON message, ToJSON message, Show message, Show model)
    => Text
    -> ElmGame model message
    -> Game

  GameBanana
    :: (FromJSON message, ToJSON message)
    => Text
    -> (  Events (Text, message)
       -> Events Key
       -> Behavior (Int, Int)
       -> Banana
            ( Behavior Scene
            , Behavior (HashSet Text)
            , Events (Text, message)
            , Events ()
            )
       )
    -> Game

gameName :: Game -> Text
gameName = \case
  GameElm    name _ -> name
  GameBanana name _ -> name


data ServerMessage
  = ServerMessage Text Value
  deriving (Show)

instance FromJSON ServerMessage where
  parseJSON =
    Aeson.withObject "ServerMessage" $ \o ->
      ServerMessage
        <$> o .: "topic"
        <*> o .: "message"


momentGame
  :: (ByteString -> IO ())
  -> Events ServerMessage
  -> Events Termbox.Event
  -> Behavior (Int, Int)
  -> Game
  -> MomentIO
       ( Behavior Scene
       , Behavior (HashSet Text)
       , Events ()
       )
momentGame send eMessage eEvent bSize = \case
  GameElm name game -> do
    (width, height) <- valueB bSize
    momentElmGame name width height send eMessage eEvent game

  GameBanana name game -> do
    eInput <- execute (parseInput <$> eMessage)

    (bScene, bSubscribe, eOutput, eDone) <-
      runBanana name (game eInput eKey bSize)

    reactimate (send . uncurry formatOutput <$> eOutput)

    pure (bScene, bSubscribe, eDone)

  where
    eKey :: Events Key
    eKey =
      filterJust
        ((\case
          EventKey key _ -> Just key
          _ -> Nothing)
          <$> eEvent)

momentElmGame
  :: forall message model.
     (FromJSON message, ToJSON message)
  => Text
  -> Int
  -> Int
  -> (ByteString -> IO ())
  -> Events ServerMessage
  -> Events Termbox.Event
  -> ElmGame model message
  -> MomentIO
       ( Behavior Scene
       , Behavior (HashSet Text)
       , Events ()
       )
momentElmGame
    name width height send eMessage eEvent
    (ElmGame init update view tickEvery subscribe) = mdo

  model0 :: model <-
    runInit width height (interpretElmIO name send) init

  let
    tickEvery0 :: Maybe Seconds
    tickEvery0 =
      tickEvery model0

  eUpdate :: Events (Maybe model) <- do
    eServerInput :: Events (Text, message) <-
      execute (parseInput <$> eMessage)

    let
      step :: model -> Input message -> MomentIO (Maybe model)
      step model input = do
        runUpdate model (interpretElmIO name send) (update input)

    let
      eInput :: Events (Input message)
      eInput =
        leftmostE
          [ eKey
          , eMouse
          , eResize
          , Tick <$> eTick
          , (\(topic, message) -> Message topic message) <$> eServerInput
          ]

    execute (step <$> bModel <@> eInput)

  let
    eModel :: Events model
    eModel =
      filterJust eUpdate

  bModel :: Behavior model <-
    stepper model0 eModel

  eTick :: Events Seconds <-
    runBanana name (momentTick tickEvery0 eTickControl)

  let
    eTickControl :: Events TickControl
    eTickControl =
      filterJust
        ((\old -> \case
          Nothing ->
            Just TickTeardown

          Just model -> do
            let new = tickEvery model
            guard (new /= old)
            pure (TickSetDelta new))
        <$> bTickEvery <@> eUpdate)

  bTickEvery :: Behavior (Maybe Seconds) <-
    stepper tickEvery0 (tickEvery <$> eModel)

  let
    eDone :: Events ()
    eDone =
      previewE _Nothing eUpdate

  let
    eScene :: Events Scene
    eScene =
      view <$> eModel

  bScene :: Behavior Scene <-
    stepper (view model0) eScene

  bSubscribe :: Behavior (HashSet Text) <-
    stepper (subscribe model0) (subscribe <$> eModel)

  pure (bScene, bSubscribe, eDone)

 where
  eKey :: Events (Input message)
  eKey =
    mapMaybeE f eEvent
    where
      f :: Termbox.Event -> Maybe (Input message)
      f = \case
        EventKey key _ -> Just (Key key)
        _ -> Nothing

  eMouse :: Events (Input message)
  eMouse =
    mapMaybeE f eEvent
    where
      f :: Termbox.Event -> Maybe (Input message)
      f = \case
        EventMouse mouse col row -> Just (Mouse mouse col row)
        _ -> Nothing

  eResize :: Events (Input message)
  eResize =
    mapMaybeE f eEvent
    where
      f :: Termbox.Event -> Maybe (Input message)
      f = \case
        EventResize col row -> Just (Resize col row)
        _ -> Nothing

interpretElmIO
  :: (ToJSON message, MonadIO m)
  => Text
  -> (ByteString -> IO ())
  -> ElmF message (m x)
  -> m x
interpretElmIO name send = \case
  Save key value k -> do
    let dir = bhaDataDir </> Text.unpack name
    let file = dir </> Text.unpack key
    liftIO $ do
      createDirectoryIfMissing True dir
      ByteString.writeFile file value
    k

  Load key k -> do
    let file = bhaDataDir </> Text.unpack name </> Text.unpack key
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

  Send topic message k -> do
    liftIO (send (formatOutput topic message))
    k

parseInput :: (FromJSON a, MonadIO m) => ServerMessage -> m (Text, a)
parseInput (ServerMessage topic value) =
  case Aeson.parseEither Aeson.parseJSON value of
    Left err ->
      throwIO (userError err)

    Right message ->
      pure (topic, message)

formatOutput :: ToJSON a => Text -> a -> ByteString
formatOutput topic message =
  (ByteString.Lazy.toStrict . Aeson.encode . Aeson.object)
    [ "type"    .= ("publish" :: Text)
    , "topic"   .= topic
    , "message" .= message
    ]
