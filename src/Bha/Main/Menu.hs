-- | The main menu to display between games.

module Bha.Main.Menu
  ( MainMenuOutput(..)
  , ᴍainMenuOutputGame
  , ᴍainMenuOutputDone
  , momentMainMenu
  ) where

import Bha.Banana.Menu
import Bha.Banana.Prelude
import Bha.Main.Game      (Game(..), gameName)

import Data.Foldable (maximum)

import qualified Data.Text      as Text
import qualified Termbox.Banana as Termbox


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
  -> Events Termbox.Event
  -> Behavior (Int, Int)
  -> m (Behavior Scene, Events MainMenuOutput)
momentMainMenu games eEvent bSize = do
  let
    eEsc   = filterE (== EventKey KeyEsc   False) eEvent
    eEnter = filterE (== EventKey KeyEnter False) eEvent

    eUp    = filterE (== EventKey (KeyChar 'k') False) eEvent
    eDown  = filterE (== EventKey (KeyChar 'j') False) eEvent

  (bMenuScene, eGame) <-
    makeMenu
      games
      (leftmostE
        [ MenuUp    <$ eUp
        , MenuDown  <$ eDown
        , MenuEnter <$ eEnter
        ])
      ((\(w, _) selected i game ->
        if selected
          then text (menucol w) (i+3) mempty mempty ("> " ++ Text.unpack (gameName game))
          else text (menucol w) (i+3) mempty mempty ("  " ++ Text.unpack (gameName game)))
      <$> bSize)

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
              [ renderTitle <$> bSize
              , bMenuScene
              ]
        <*> pure NoCursor

  pure (bScene, eOutput)
  where
    menucol :: Int -> Int
    menucol w =
      div w 2 - div (maximum (map (Text.length . gameName) games) + 2) 2

renderTitle :: (Int, Int) -> Cells
renderTitle (w, _h) =
  text (div w 2 - div (length s) 2) 1 mempty mempty s
  where
    s :: [Char]
    s = "* Welcome to the Boston Haskell Arcade! *"
