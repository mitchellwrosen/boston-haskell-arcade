{-# LANGUAGE DeriveFunctor, LambdaCase, NoImplicitPrelude #-}

module Bha.Game
  ( GameOutput(..)
  , ɢameOver
  ) where

import Bha.Prelude

data GameOutput a
  = GameOver (Maybe a)
  deriving (Eq, Functor)

ɢameOver :: Prism' (GameOutput a) (Maybe a)
ɢameOver = prism' GameOver (\case { GameOver x -> Just x; _ -> Nothing })
