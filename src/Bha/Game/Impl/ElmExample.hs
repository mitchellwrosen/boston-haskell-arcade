-- | Example Elm-style game.

{-# LANGUAGE NoImplicitPrelude #-}

module Bha.Game.Impl.ElmExample
  ( game
  ) where

import Termbox.Banana (Cells, Cursor(..), Event(..), Key(..), Scene(..))

import Bha.Game    (ElmGame(ElmGame), Game(..))
import Bha.Prelude
import Bha.View

data Model
  = Model
      !Int             -- Event count
      !NominalDiffTime -- Elapsed time

game :: Game
game =
  GameElm (ElmGame init update view isDone tickEvery)

init :: Model
init =
  Model 0 0

update :: Either NominalDiffTime Event -> Model -> Model
update event (Model n elapsed) =
  case event of
    -- Fast-forward on esc to quit!
    Right (EventKey KeyEsc _) ->
      Model 11 elapsed

    Right _ ->
      Model (n + 1) elapsed

    Left delta ->
      Model n (elapsed + delta)

view :: Model -> Scene
view (Model n elapsed) =
  let
    cells :: Cells
    cells =
      mconcat
        [ tbstr 0 0 mempty mempty "I am an Elm game!"
        , tbstr 0 2 mempty mempty "Let's count to 10."
        , tbstr 2 4 mempty mempty (show n)
        , tbstr 0 6 mempty mempty ("Elapsed time: " ++ show elapsed)
        ]
  in
    Scene cells NoCursor

isDone :: Model -> Bool
isDone (Model n _) =
  n > 10

tickEvery :: Model -> Maybe NominalDiffTime
tickEvery _ =
  Just 1
