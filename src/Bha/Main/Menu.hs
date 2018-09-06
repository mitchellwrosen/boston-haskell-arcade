-- | The main menu to display between games.

module Bha.Main.Menu
  ( MainMenuOutput(..)
  , ᴍainMenuOutputGame
  , ᴍainMenuOutputDone
  , momentMainMenu
  ) where

import Bha.Banana.Menu
import Bha.Banana.Prelude
import Bha.Main.Game (Game(..), gameName)

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
  => [Game] -- ^ Non-empty game list.
  -> Events TermEvent
  -> m (Behavior Scene, Events MainMenuOutput)
momentMainMenu games eEvent = do
  let
    eEsc   = filterE (== EventKey KeyEsc   False) eEvent
    eEnter = filterE (== EventKey KeyEnter False) eEvent

    eUp    = filterE (== EventKey (KeyChar 'k') False) eEvent
    eDown  = filterE (== EventKey (KeyChar 'j') False) eEvent

  (bMenuScene, eGame) <-
    makeMenu
      (\selected i game ->
        if selected
          then text 0 (i+2) mempty mempty ("> " ++ gameName game)
          else text 0 (i+2) mempty mempty ("  " ++ gameName game))
      games
      (leftmostE
        [ MenuUp    <$ eUp
        , MenuDown  <$ eDown
        , MenuEnter <$ eEnter
        ])

  let
    eOutput :: Events MainMenuOutput
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
              , bMenuScene
              ]
        <*> pure NoCursor

  pure (bScene, eOutput)

renderTitle :: Cells
renderTitle =
  text 0 0 mempty mempty "* Welcome to the Boston Haskell Arcade! *"
