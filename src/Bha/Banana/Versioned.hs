module Bha.Banana.Versioned
  ( Versioned
  , Migrate(..)
  , save
  , load
    -- * Re-exports
  , Serialize
  ) where

import Control.Monad.Reader
import Data.Serialize             (Serialize)
import Reactive.Banana.Frameworks (reactimate)
import System.Directory           (createDirectoryIfMissing)
import System.FilePath            ((</>))

import qualified Data.ByteString as ByteString

import Bha.Prelude
import Internal.Bha.Banana.Prelude
import Internal.Bha.Versioned

save
  :: forall a as.
     Versioned as a
  => FilePath
  -> Events a
  -> Banana ()
save path eValue =
  Banana $ ReaderT $ \name ->
    reactimate (doSave name <$> eValue)

 where
  doSave :: [Char] -> a -> IO ()
  doSave name value = do
    let dir = bhaDataDir </> name
    createDirectoryIfMissing True dir
    ByteString.writeFile (dir </> path) (encodeVersioned value)

load :: Versioned as a => FilePath -> Banana (Maybe a)
load path =
  Banana $ ReaderT $ \name -> liftIO $ do
    asum
      [ either (const Nothing) Just . decodeVersioned <$>
          ByteString.readFile (bhaDataDir </> name </> path)
      , pure Nothing
      ]
