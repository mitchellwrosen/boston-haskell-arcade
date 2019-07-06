module Bha.Prelude
  ( Seconds
  , bhaDataDir
  , debug
  , module X
  ) where

import Internal.Bha.Orphans as X ()

import Control.Applicative       as X (Alternative(empty, (<|>)))
import Control.Category          as X ((>>>))
import Control.Concurrent        as X (forkIO, threadDelay)
import Control.Concurrent.STM    as X (STM, TMVar, TVar, atomically,
                                       newEmptyTMVarIO, newTVarIO, readTMVar,
                                       readTVar, tryPutTMVar, writeTVar)
import Control.Lens              as X (Lens, Lens', Prism', Traversal,
                                       Traversal', foldMapOf, ix, mapped, over,
                                       preview, prism', snoc, traversed, (%~),
                                       (.~), (^.), (^?), _1, _2, _3, _Just,
                                       _Nothing)
import Control.Lens.Extras       as X (is)
import Control.Monad             as X (forever, guard, unless, void, when)
import Control.Monad.IO.Class    as X (MonadIO(..))
import Control.Monad.Trans.Class as X (MonadTrans(..))
import Control.Monad.Trans.Maybe as X (MaybeT(..))
import Data.ByteString           as X (ByteString)
import Data.Foldable             as X (asum, fold, for_, toList)
import Data.Function             as X (fix, (&))
import Data.HashMap.Strict       as X (HashMap)
import Data.HashSet              as X (HashSet)
import Data.Generics.Labels      as X ()
import Data.Kind                 as X (Type)
import Data.Maybe                as X (fromMaybe)
import Data.Maybe                as X (catMaybes)
import Data.Sequence             as X (Seq)
import Data.Set                  as X (Set)
import Data.Text                 as X (Text)
import Data.Text                 (pack)
import Data.Text.IO              (appendFile)
import Data.Time                 (NominalDiffTime)
import Data.Void                 as X (Void)
import GHC.Generics              as X (Generic)
import Prelude                   as X hiding (appendFile, fail, head, init,
                                       tail)
import System.Directory          (XdgDirectory(..), getXdgDirectory)
import System.IO.Unsafe          (unsafePerformIO)
import UnliftIO.Exception        as X (throwIO)


type Seconds
  = NominalDiffTime

-- | Append a debug message to "bha-debug.txt".
debug :: (MonadIO m, Show a) => Text -> a -> m ()
debug s x =
  liftIO (appendFile "bha-debug.txt" (s  <> ": " <> pack (show x) <> "\n"))

bhaDataDir :: FilePath
bhaDataDir =
  unsafePerformIO (getXdgDirectory XdgData "boston-haskell-arcade")
{-# NOINLINE bhaDataDir #-}
