{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, ScopedTypeVariables #-}

module Bha.Main
  ( main
  ) where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Termbox.Banana             (Attr, Cell(..), Cells, Cursor(..),
                                   InputMode(..), Key(..), MouseMode(..),
                                   OutputMode(..), Scene(..))

import qualified Termbox.Banana as Tb

import Bha.Prelude

main :: IO ()
main =
  Tb.main (InputModeEsc MouseModeYes) OutputModeNormal main'

main'
  :: Event Tb.Event
  -> Behavior (Int, Int)
  -> MomentIO (Behavior Tb.Scene, Event ())
main' eEvent _bSize = do
  let
    eDone :: Event ()
    eDone =
      () <$ filterE (== Tb.EventKey KeyEsc False) eEvent

  bIndex :: Behavior Int <-
    accumB 0 $ unions
      [ (+1)       <$ filterE (== Tb.EventKey (KeyChar 'j') False) eEvent
      , subtract 1 <$ filterE (== Tb.EventKey (KeyChar 'k') False) eEvent
      ]

  let
    bCells :: Behavior Cells
    bCells =
      mconcat
        [ pure
            (tbstr 0 0 mempty mempty
              "* Welcome to the Boston Haskell Arcade! *")
        , render <$> bIndex
        ]
     where
      render :: Int -> Cells
      render i =
        foldMap
          (\(j, (name, _game)) ->
            if i `mod` length gamelist == j
              then tbstr 0 (j+2) mempty mempty ("> " ++ name)
              else tbstr 0 (j+2) mempty mempty ("  " ++ name))
          (zip [0..] gamelist)

  let
    bScene :: Behavior Scene
    bScene =
      Scene
        <$> bCells
        <*> pure NoCursor

  pure (bScene, eDone)

tbstr :: Int -> Int -> Attr -> Attr -> [Char] -> Cells
tbstr col0 row fg bg =
  foldMap (\(col, c) -> Tb.set col row (Cell c fg bg)) . zip [col0..]

gamelist :: [([Char], Game)]
gamelist =
  [ ("Game 1", Game)
  , ("Game 2", Game)
  ]

data Game
  = Game
