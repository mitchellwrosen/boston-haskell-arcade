module Bha.Banana.Menu
  ( MenuControl(..)
  , makeMenu
  ) where

import Bha.Banana.Prelude

data MenuControl
  = MenuUp
  | MenuDown
  | MenuEnter
  deriving Eq

makeMenu
  :: forall a m.
     MonadMoment m
  => (Bool -> Int -> a -> Cells)
  -> [a]
  -> Events MenuControl
  -> m (Behavior Cells, Events a)
makeMenu render menu0 eControl = mdo
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
      (menu0 !!) <$> bIndex <@ eEnter

  let
    bCells :: Behavior Cells
    bCells =
      f <$> bIndex
     where
      f :: Int -> Cells
      f i =
        foldMap
          (\(j, x) -> render (i == j) j x)
          (zip [0..] menu0)

  pure (bCells, eEntered)
