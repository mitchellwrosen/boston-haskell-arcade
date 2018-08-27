-- | Example Elm-style game.

{-# LANGUAGE NoImplicitPrelude, NamedFieldPuns, RecordWildCards #-}

module Bha.Game.Impl.ElmExample
  ( game
  ) where

import Bha.Elm.Prelude
import Bha.View

data Model
  = Model
      !Int             -- Event count
      !NominalDiffTime -- Elapsed time

game :: ElmGame Model
game =
  ElmGame init update view tickEvery

init :: Seed -> Model
init _ =
  Model 0 0

update :: Either NominalDiffTime Event -> Model -> Maybe Model
update event (Model n elapsed) =
  case event of
    Right (EventKey KeyEsc _) ->
      Nothing

    Right _ -> do
      guard (n < 10)
      pure (Model (n + 1) elapsed)

    Left delta ->
      Just (Model n (elapsed + delta))

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

tickEvery :: Model -> Maybe NominalDiffTime
tickEvery _ =
  Just 1
