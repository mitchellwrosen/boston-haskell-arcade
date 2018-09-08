module Bha.Banana.Versioned
  ( Versioned
  , Migrate(..)
  , save
  , load
    -- * Re-exports
  , Serialize
  ) where

import Data.Serialize   (Serialize)
import System.Directory (createDirectoryIfMissing)
import System.FilePath  ((</>))

import qualified Data.ByteString as ByteString

import Bha.Prelude
import Internal.Bha.Banana.Prelude
import Internal.Bha.Versioned

-- TODO banana load/save implicit file path


save :: Versioned as a => FilePath -> FilePath -> a -> IO ()
save dir key value = do
  createDirectoryIfMissing True dir
  ByteString.writeFile (dir </> key) (encodeVersioned value)

load :: Versioned as a => FilePath -> Banana (Maybe a)
load path = Banana . liftIO $ do
  asum
    [ either (const Nothing) Just . decodeVersioned <$>
        ByteString.readFile path
    , pure Nothing
    ]
