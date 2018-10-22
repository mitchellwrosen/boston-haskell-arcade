module Internal.Bha.Debug
  ( debug
  ) where

import File.Text (appendFile)
import Mitchell.Prelude
import Text (pack)

debug :: (MonadIO m, Show a) => Text -> a -> m ()
debug s x =
  liftIO (appendFile "bha-debug.txt" (s  <> ": " <> pack (show x) <> "\n"))
