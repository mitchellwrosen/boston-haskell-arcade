-- | Example game built with the Elm architecture.

{-# LANGUAGE NoImplicitPrelude #-}

module Bha.Game.Impl.ElmExample
  ( game
  ) where

import Termbox.Banana (Cells, Cursor(..), Key(..), Scene(..))

import qualified Termbox.Banana as Tb

import Bha.Game    (ElmGame(ElmGame), Game(..))
import Bha.Prelude
import Bha.View

type Model
  = Int

game :: Game
game =
  GameElm (ElmGame init update view isDone tickEvery)

init :: Model
init =
  0

update :: Either () Tb.Event -> Model -> Model
update event n =
  case event of
    -- Fast-forward on esc to quit!
    Right (Tb.EventKey KeyEsc _) ->
      11

    _ ->
      n + 1

view :: Model -> Scene
view n =
  let
    cells :: Cells
    cells =
      mconcat
        [ tbstr 0 0 mempty mempty "I am an Elm game!"
        , tbstr 0 2 mempty mempty "Let's count to 10."
        , tbstr 2 4 mempty mempty (show n)
        ]
  in
    Scene cells NoCursor

isDone :: Model -> Bool
isDone n =
  n > 10

tickEvery :: Model -> Maybe Double
tickEvery _ =
  Nothing
