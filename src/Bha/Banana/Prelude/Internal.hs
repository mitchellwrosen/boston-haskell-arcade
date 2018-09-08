-- TODO move Bha.Banana.Prelude to Bha.Banana.Prelude.Internal

module Bha.Banana.Prelude.Internal
  ( Banana(..)
  , reactimate
  , load
  , save
  ) where

import Reactive.Banana
import Reactive.Banana.Frameworks (MomentIO)
import System.Directory           (createDirectoryIfMissing)
import System.FilePath            ((</>))

import qualified Data.ByteString            as ByteString
import qualified Reactive.Banana.Frameworks as Reactive.Banana

import Bha.Prelude
import Internal.Bha.Versioned

-- | A wrapper around MomentIO, used to control what effects games are allowed
-- to use.
newtype Banana a
  = Banana { unBanana :: MomentIO a }
  deriving newtype (Applicative, Functor, Monad, MonadFix, MonadMoment)

-- TODO IO wrapper

reactimate :: Event (IO ()) -> Banana ()
reactimate =
  Banana . Reactive.Banana.reactimate


-- TODO banana load/save implicit file path

load :: Versioned as a => FilePath -> Banana (Maybe a)
load path = Banana . liftIO $ do
  asum
    [ either (const Nothing) Just . decodeVersioned <$>
        ByteString.readFile path
    , pure Nothing
    ]

save :: Versioned as a => FilePath -> FilePath -> a -> IO ()
save dir key value = do
  createDirectoryIfMissing True dir
  ByteString.writeFile (dir </> key) (encodeVersioned value)
