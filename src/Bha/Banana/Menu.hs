module Bha.Banana.Menu
  ( MenuControl(..)
  , makeMenu
  ) where

import Bha.Banana.Prelude
import Control.Lens ((^?!))

import Data.List    (zip)


data MenuControl
  = MenuUp
  | MenuDown
  | MenuEnter
  deriving stock (Eq)

makeMenu
  :: forall a m.
     MonadMoment m
  => [a]
  -> Events MenuControl
  -> Behavior (Bool -> Int -> a -> Cells)
  -> m (Behavior Cells, Events a)
makeMenu menu0 eControl bRender = mdo
  let
    eUp    = filterE (== MenuUp)    eControl
    eDown  = filterE (== MenuDown)  eControl
    eEnter = filterE (== MenuEnter) eControl

  bIndex :: Behavior Int <-
    accumB 0 (unions
      [ max 0 . subtract 1 <$ eUp
      , min (length menu0 - 1) . (+1) <$ eDown
      ])

  let
    eEntered :: Events a
    eEntered =
      (\i -> menu0 ^?! ix i) <$> bIndex <@ eEnter

  let
    bCells :: Behavior Cells
    bCells =
      f <$> bRender <*> bIndex
     where
      f :: (Bool -> Int -> a -> Cells) -> Int -> Cells
      f render i =
        foldMap
          (\(j, x) -> render (i == j) j x)
          (zip [0..] menu0)

  pure (bCells, eEntered)
