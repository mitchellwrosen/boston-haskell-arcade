{-# LANGUAGE NoImplicitPrelude #-}

module Bha.Elm.Prelude.Internal
  ( Seed(..)
  ) where

import System.Random (StdGen)

newtype Seed
  = Seed StdGen
