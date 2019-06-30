{-# LANGUAGE DeriveAnyClass, DerivingStrategies, DuplicateRecordFields,
             InstanceSigs, LambdaCase, OverloadedStrings, ScopedTypeVariables,
             ViewPatterns #-}

import Control.Concurrent.Async       (race_)
import Control.Concurrent.STM         (atomically)
import Control.Concurrent.STM.TChan
import Control.Exception              (Exception, throwIO)
import Control.Monad                  (forever, when)
import Data.Aeson                     ((.:))
import Data.ByteString                (ByteString)
import Data.HashSet                   (HashSet)
import Data.IORef
import Data.IORef                     (modifyIORef', newIORef, readIORef)
import Data.Text                      (Text)
import Data.Text.IO                   (hPutStrLn)
import Network.HTTP.Types             (status400, status500)
import Network.Socket                 (SockAddr)
import Network.Wai                    (Request, remoteHost, responseLBS,
                                       responseRaw)
import Network.Wai.Handler.Warp       (run)
import Network.Wai.Handler.WebSockets (getRequestHead, isWebSocketsReq,
                                       runWebSockets)
import Network.WebSockets             (Connection, PendingConnection,
                                       acceptRequest, defaultConnectionOptions,
                                       receiveData, sendTextData)
import System.Environment             (getArgs)
import System.Exit                    (exitFailure)
import System.IO                      (stderr)
import Text.Read                      (readMaybe)

import qualified Data.Aeson       as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashSet     as HashSet


data Message
  = Subscribe !(HashSet Text)
  | Unsubscribe !(HashSet Text)
  | Publish !Text !Aeson.Value


instance Aeson.FromJSON Message where
  parseJSON :: Aeson.Value -> Aeson.Parser Message
  parseJSON =
    Aeson.withObject "message" $ \o ->
      (o .: "type") >>= \case
        "subscribe" ->
          Subscribe
            <$> o .: "topics"

        "unsubscribe" ->
          Unsubscribe
            <$> o .: "topics"

        "publish" ->
          Publish
            <$> o .: "topic"
            <*> o .: "message"

        s ->
          fail ("Unexpected message type: " <> s)


data MalformedMessage
  = MalformedMessage !SockAddr !ByteString
  deriving stock (Show)
  deriving anyclass (Exception)


main :: IO ()
main = do
  port :: Int <-
    parseArgs =<< getArgs

  chan :: TChan (SockAddr, Text, Aeson.Value) <-
    newBroadcastTChanIO

  run port $ \req resp ->
    if isWebSocketsReq req
      then
        resp
          (responseRaw
            (runWebSockets
              defaultConnectionOptions
              (getRequestHead req)
              (wsApp chan req))
            (responseLBS status500 [] ""))
      else
        resp (responseLBS status400 [] "")
 where
  parseArgs :: [String] -> IO Int
  parseArgs = \case
    [readMaybe -> Just port] ->
      pure port
    _ -> do
      hPutStrLn stderr "Usage: generic-websocket-server PORT"
      exitFailure

wsApp
  :: TChan (SockAddr, Text, Aeson.Value)
  -> Request
  -> PendingConnection
  -> IO ()
wsApp chan request pconn = do
  conn :: Connection <-
    acceptRequest pconn

  chan' :: TChan (SockAddr, Text, Aeson.Value) <-
    atomically (dupTChan chan)

  subscribedRef :: IORef (HashSet Text) <-
    newIORef mempty

  -- Send thread: send messages to the connected client that
  --   * Are of a topic the client has subscribed to
  --   * Are not from the client itself
  let send :: IO ()
      send = do
        (sender, topic, message) <-
          atomically (readTChan chan')

        subscribed :: HashSet Text <-
          readIORef subscribedRef

        when (sender /= remoteHost request && topic `elem` subscribed) $
          (sendTextData conn . Aeson.encode)
            (Aeson.object
              [ ("topic", Aeson.toJSON topic)
              , ("message", message)
              ])

  -- Receive thread: handle subscribe messages and payload messages coming from
  -- the client.
  let recv :: IO ()
      recv = do
        bytes :: ByteString <-
          receiveData conn

        case Aeson.decodeStrict' bytes of
          Nothing ->
            throwIO (MalformedMessage (remoteHost request) bytes)

          Just message ->
            case message of
              Subscribe topics ->
                modifyIORef' subscribedRef (HashSet.union topics)

              Unsubscribe topics ->
                modifyIORef' subscribedRef (`HashSet.difference` topics)

              Publish topic message' ->
                atomically
                  (writeTChan chan (remoteHost request, topic, message'))

  race_ (forever send) (forever recv)
