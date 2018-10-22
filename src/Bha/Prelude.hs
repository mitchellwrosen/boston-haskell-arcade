module Bha.Prelude
  ( Seconds
  , (∘)
  , (⨾)
  , bhaDataDir
  , module X
  ) where

import Internal.Bha.Debug   as X (debug)
import Internal.Bha.Orphans as X ()
import Mitchell.Prelude     as X hiding (set, view)
import Type                 as X (Type)

import File             (FilePath, XdgDirectory(..), getXdgDirectory)
import System.IO.Unsafe (unsafePerformIO)
import Time             (NominalDiffTime)

type Seconds
  = NominalDiffTime

(∘) :: Category p => p b c -> p a b -> p a c
(∘) = (<<<)

(⨾) :: Category p => p a b -> p b c -> p a c
(⨾) = (>>>)

bhaDataDir :: FilePath
bhaDataDir =
  unsafePerformIO (getXdgDirectory XdgData "boston-haskell-arcade")
{-# NOINLINE bhaDataDir #-}
