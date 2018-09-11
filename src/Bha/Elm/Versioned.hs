module Bha.Elm.Versioned
  ( Versioned
  , Migrate(..)
  , load
  , save
    -- * Re-exports
  , Serialize
  ) where

import Data.Serialize (Serialize)

import Bha.Prelude
import Internal.Bha.Elm.Prelude
import Internal.Bha.Versioned

save :: (MonadElm message m, Versioned as a) => Text -> a -> m ()
save k v =
  interpretElm (Save k (encodeVersioned v) ())

load
  :: forall a as m message.
     (MonadElm message m, Versioned as a)
  => Text
  -> m (Maybe a)
load k =
  interpretElm (Load k f)
 where
  f :: Maybe ByteString -> Maybe a
  f mbytes = do
    bytes <- mbytes
    Right val <- pure (decodeVersioned bytes)
    pure val
