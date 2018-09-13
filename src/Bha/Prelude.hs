module Bha.Prelude
  ( bhaDataDir
  , module X
  ) where

import Control.Applicative       as X (Alternative, empty, (<|>))
import Control.Concurrent        as X (forkIO, threadDelay)
import Control.Concurrent.STM    as X
import Control.Lens              as X (Prism', Traversal', ix, mapped, over,
                                       preview, prism', snoc, (-=), (^.), (^?),
                                       _1, _2, _3, _4)
import Control.Monad             as X
import Control.Monad.Fix         as X
import Control.Monad.IO.Class    as X
import Control.Monad.Trans.Maybe as X
import Data.ByteString           as X (ByteString)
import Data.Foldable             as X (asum, find, for_, toList)
import Data.Function             as X (fix, (&))
import Data.HashMap.Strict       as X (HashMap)
import Data.HashSet              as X (HashSet)
import Data.IntSet               as X (IntSet)
import Data.IORef                as X
import Data.Kind                 as X (Type)
import Data.Maybe                as X hiding (fromJust)
import Data.Proxy                as X (Proxy(Proxy))
import Data.Set                  as X (Set)
import Data.Text                 as X (Text)
import Data.Time                 as X (NominalDiffTime, UTCTime, getCurrentTime)
import Data.Void                 as X (Void)
import Data.Word                 as X
import GHC.Generics              as X (Generic)
-- TODO Don't export bad bits of Prelude
import Prelude as X hiding (init)

import Internal.Bha.Debug   as X (debug)
import Internal.Bha.Orphans as X ()

import System.Directory (XdgDirectory(..), getXdgDirectory)
import System.IO.Unsafe (unsafePerformIO)

bhaDataDir :: FilePath
bhaDataDir =
  unsafePerformIO (getXdgDirectory XdgData "boston-haskell-arcade")
{-# NOINLINE bhaDataDir #-}
