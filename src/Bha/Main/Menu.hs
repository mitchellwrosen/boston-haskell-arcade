-- | The main menu to display between games.

{-# LANGUAGE LambdaCase, NoImplicitPrelude, ScopedTypeVariables #-}

module Bha.Main.Menu
  ( MainMenuOutput(..)
  , ᴍainMenuOutputGame
  , ᴍainMenuOutputDone
  , momentMainMenu
  ) where

import Reactive.Banana
import Termbox.Banana  (Cells, Cursor(..), Key(..), Scene(..))

import qualified Termbox.Banana as Tb

import Bha.Game
import Bha.Prelude
import Bha.View

data MainMenuOutput
  = MainMenuOutputGame Game -- ^ A game was selected.
  | MainMenuOutputDone      -- ^ Quit the program.

ᴍainMenuOutputGame :: Prism' MainMenuOutput Game
ᴍainMenuOutputGame = prism' MainMenuOutputGame (\case { MainMenuOutputGame x -> Just x; _ -> Nothing })

ᴍainMenuOutputDone :: Prism' MainMenuOutput ()
ᴍainMenuOutputDone = prism' (const MainMenuOutputDone) (\case { MainMenuOutputDone -> Just (); _ -> Nothing })

-- | Create a main menu in this moment.
momentMainMenu
  :: MonadMoment m
  => [([Char], Game)] -- ^ Non-empty game list.
  -> Event Tb.Event
  -> m (Behavior Scene, Event MainMenuOutput)
momentMainMenu games eEvent = do
  let
    eEsc   = filterE (== Tb.EventKey KeyEsc   False) eEvent
    eEnter = filterE (== Tb.EventKey KeyEnter False) eEvent

  -- The currently-selected index of the game list.
  -- TODO: build a reusable zipper component with up/down/select controls
  bIndex :: Behavior Int <-
    accumB 0 $ unions
      [ min (length games - 1) . (+1)
          <$ filterE (== Tb.EventKey (KeyChar 'j') False) eEvent
      , max 0 . subtract 1
          <$ filterE (== Tb.EventKey (KeyChar 'k') False) eEvent
      ]

  let
    eGame :: Event Game
    eGame =
      snd . (games !!) <$> bIndex <@ eEnter

  let
    eOutput :: Event MainMenuOutput
    eOutput =
      unionWith const
        (MainMenuOutputDone <$  eEsc)
        (MainMenuOutputGame <$> eGame)

  let
    bScene :: Behavior Scene
    bScene =
      Scene
        <$> mconcat
              [ pure renderTitle
              , renderGameList games <$> bIndex
              ]
        <*> pure NoCursor

  pure (bScene, eOutput)

renderTitle :: Cells
renderTitle =
  tbstr 0 0 mempty mempty "* Welcome to the Boston Haskell Arcade! *"

renderGameList :: [([Char], Game)] -> Int -> Cells
renderGameList games i =
  foldMap
    (\(j, (name, _game)) ->
      if i == j
        then tbstr 0 (j+2) mempty mempty ("> " ++ name)
        else tbstr 0 (j+2) mempty mempty ("  " ++ name))
    (zip [0..] games)
