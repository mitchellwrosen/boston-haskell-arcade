module Bha.Elm.Versioned
  ( Versioned
  , Migrate(..)
  , load
  , save
    -- * Re-exports
  , Serialize
  ) where

import Bha.Internal.Elm.Prelude
import Bha.Internal.Versioned
import Bha.Prelude

import Data.Serialize (Serialize)


-- | Save a versioned the given name.
save :: (MonadElm message m, Versioned as a) => Text -> a -> m ()
save k v =
  interpretElm (Save k (encodeVersioned v) ())

-- | Load a value by name.
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
