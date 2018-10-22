module Bha.Banana.Versioned
  ( Versioned
  , Migrate(..)
  , save
  , load
    -- * Re-exports
  , Serialize
  ) where

import Data.Serialize (Serialize)
import File           (FilePath, createDirectoryIfMissing, (</>))
import FRP            (reactimate)
import Reader

import qualified File.Binary

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
    File.Binary.writeFile (dir </> path) (encodeVersioned value)

load :: Versioned as a => FilePath -> Banana (Maybe a)
load path =
  Banana $ ReaderT $ \name -> liftIO $ do
    asum
      [ either (const Nothing) Just . decodeVersioned <$>
          File.Binary.readFile (bhaDataDir </> name </> path)
      , pure Nothing
      ]
