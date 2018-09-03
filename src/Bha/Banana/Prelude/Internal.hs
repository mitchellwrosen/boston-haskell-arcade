{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving,
             NoImplicitPrelude #-}

module Bha.Banana.Prelude.Internal
  ( Banana(..)
  , load
  , save
  ) where

import Data.Serialize             (Serialize)
import Reactive.Banana
import Reactive.Banana.Frameworks

import qualified Data.ByteString as ByteString
import qualified Data.Serialize  as Serialize

import Bha.Prelude

-- | A wrapper around MomentIO, used to control what effects games are allowed
-- to use.
newtype Banana a
  = Banana { unBanana :: MomentIO a }
  deriving newtype (Applicative, Functor, Monad, MonadFix, MonadMoment)

-- TODO banana load/save implicit file path

load :: Serialize a => FilePath -> Banana (Maybe a)
load path = Banana . liftIO $ do
  asum
    [ either (const Nothing) Just . Serialize.decode <$>
        ByteString.readFile path
    , pure Nothing
    ]

save :: Serialize a => FilePath -> a -> Banana ()
save path value = Banana . liftIO $
  ByteString.writeFile path (Serialize.encode value)
