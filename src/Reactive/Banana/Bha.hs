{-# LANGUAGE LambdaCase, NoImplicitPrelude, RankNTypes #-}

module Reactive.Banana.Bha
  ( Events
  , filterCoincidentE
  , leftmostE
  , mergeE
  , previewE
  , unpairE
  , module Reactive.Banana
  ) where

import Reactive.Banana hiding (Event)

import qualified Reactive.Banana

import Bha.Prelude

type Events
  = Reactive.Banana.Event

filterCoincidentE :: Events b -> Events a -> Events a
filterCoincidentE e1 e2 =
  filterJust (mergeE (const Nothing) (const Nothing) (const Just) e1 e2)

leftmostE :: [Events a] -> Events a
leftmostE =
  foldr (unionWith const) never

data Merge a b
  = MergeL a
  | MergeR b
  | MergeLR a b

mergeE
  :: (a -> c)
  -> (b -> c)
  -> (a -> b -> c)
  -> Events a
  -> Events b
  -> Events c
mergeE f g h xs ys =
  (\case
    MergeL x -> f x
    MergeR y -> g y
    MergeLR x y -> h x y)
  <$>
  unionWith
    (\(MergeL x) (MergeR y) -> MergeLR x y)
    (MergeL <$> xs)
    (MergeR <$> ys)

-- | Filter an 'Event' with a 'Prism''.
previewE :: Prism' s a -> Events s -> Events a
previewE p e =
  filterJust (preview p <$> e)

-- | Split an event of @(a, b)@ into two coindicent events of @a@ and @b@.
unpairE :: Events (a, b) -> (Events a, Events b)
unpairE e =
  (fst <$> e, snd <$> e)
