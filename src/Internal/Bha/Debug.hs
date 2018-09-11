module Internal.Bha.Debug
  ( debug
  ) where

import Control.Monad.IO.Class
import Prelude

debug :: (MonadIO m, Show a) => String -> a -> m ()
debug s x =
  liftIO (appendFile "bha-debug.txt" (s  ++ ": " ++ show x ++ "\n"))
