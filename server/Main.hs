{-# LANGUAGE DeriveAnyClass, DerivingStrategies, DuplicateRecordFields,
             InstanceSigs, LambdaCase, OverloadedStrings, ScopedTypeVariables,
             ViewPatterns #-}

import Control.Concurrent.Async       (race_)
import Control.Concurrent.MVar
import Control.Concurrent.STM         (atomically)
import Control.Concurrent.STM.TChan
import Control.Exception              (Exception, catch, throwIO)
import Control.Monad                  (forever, when)
import Data.Aeson                     ((.:))
import Data.ByteString                (ByteString)
import Data.HashSet                   (HashSet)
import Data.IORef
import Data.IORef                     (modifyIORef', newIORef, readIORef)
import Data.Text                      (Text, unpack)
import Data.Text.Encoding             (decodeUtf8)
import Data.Text.IO                   (hPutStrLn)
import Network.HTTP.Types             (status400, status500)
import Network.Socket                 (SockAddr(..), hostAddressToTuple)
import Network.Wai                    (Request, remoteHost, responseLBS,
                                       responseRaw)
import Network.Wai.Handler.Warp       (run)
import Network.Wai.Handler.WebSockets (getRequestHead, isWebSocketsReq,
                                       runWebSockets)
import Network.WebSockets             (Connection,
                                       ConnectionException(ConnectionClosed),
                                       PendingConnection, acceptRequest,
                                       defaultConnectionOptions, receiveData,
                                       sendTextData)
import System.Environment             (getArgs)
import System.Exit                    (exitFailure)
import System.IO                      (stderr)
import System.IO.Unsafe               (unsafePerformIO)
import Text.Printf                    (printf)
import Text.Read                      (readMaybe)

import qualified Data.Aeson           as Aeson
import qualified Data.Aeson.Types     as Aeson
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Data.HashSet         as HashSet


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


lock :: MVar ()
lock =
  unsafePerformIO (newMVar ())
{-# NOINLINE lock #-}

sync :: IO a -> IO a
sync action =
  withMVar lock (const action)


main :: IO ()
main = do
  port :: Int <-
    parseArgs =<< getArgs

  chan :: TChan (SockAddr, Text, Aeson.Value) <-
    newBroadcastTChanIO

  printf "Running on port %d\n" port

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
        hPutStrLn stderr "Usage: boston-haskell-arcade-server PORT"
        exitFailure

wsApp
  :: TChan (SockAddr, Text, Aeson.Value)
  -> Request
  -> PendingConnection
  -> IO ()
wsApp chan request pconn = do
  conn :: Connection <-
    acceptRequest pconn

  sync (printf "Client %s connected\n" clientId)

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
              Subscribe topics -> do
                sync
                  (printf
                    "Client %s subscribed to %s\n"
                    clientId
                    (show (HashSet.toList topics)))

                modifyIORef' subscribedRef (HashSet.union topics)

              Unsubscribe topics -> do
                sync
                  (printf
                    "Client %s unsubscribed from %s\n"
                    clientId
                    (show (HashSet.toList topics)))

                modifyIORef' subscribedRef (`HashSet.difference` topics)

              Publish topic message' -> do
                sync
                  (printf
                    "Client %s published to %s: %s\n"
                    clientId
                    (show topic)
                    (unpack
                      (decodeUtf8
                        (ByteString.Lazy.toStrict
                          (Aeson.encode message')))))

                atomically
                  (writeTChan chan (remoteHost request, topic, message'))

  race_ (forever send) (forever recv)
    `catch`
      \case
        ConnectionClosed -> sync (printf "Client %s disconnected\n" clientId)
        e -> throwIO e

  where
    clientId :: String
    clientId =
      case remoteHost request of
        SockAddrInet port host ->
          case hostAddressToTuple host of
            (w1, w2, w3, w4) ->
              show w1 ++ "." ++ show w2 ++ "." ++ show w3 ++ "." ++ show w4 ++ ":" ++ show port
        s ->
          error (show s)
