-- | Example Elm-style game.

{-# LANGUAGE TemplateHaskell #-}

module Bha.Game.Impl.ElmExample
  ( game
  ) where

import Bha.Elm.Prelude


--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

data Model
  = Model
  { count   :: Int
  , elapsed :: Seconds
  } deriving stock (Generic, Show)

init :: Int -> Int -> Init Void Model
init _ _ =
  pure Model
    { count = 0
    , elapsed = 0
    }


--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------

update :: Input Void -> Update Model Void ()
update = \case
  Key KeyEsc ->
    gameover

  Key _ -> do
    n <- use #count
    guard (n < 10)
    #count .= (n+1)

  Tick delta ->
    #elapsed %= (+ delta)

  _ -> do
    pure ()


--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

view :: Model -> Scene
view (Model n elapsed) =
  let
    cells :: Cells
    cells =
      mconcat
        [ text 0 0 mempty mempty "I am an Elm game!"
        , text 0 2 mempty mempty "Let's count to 10."
        , text 2 4 mempty mempty (show n)
        , text 0 6 mempty mempty ("Elapsed time: " ++ show elapsed)
        ]
  in
    Scene cells NoCursor


--------------------------------------------------------------------------------
-- Tick
--------------------------------------------------------------------------------

-- Tick once per second.
tickEvery :: Model -> Maybe Seconds
tickEvery _ =
  Just 1


--------------------------------------------------------------------------------
-- Subscribe
--------------------------------------------------------------------------------

subscribe :: Model -> HashSet Text
subscribe _ =
  mempty


--------------------------------------------------------------------------------
-- Game
--------------------------------------------------------------------------------

game :: ElmGame Model Void
game =
  ElmGame init update view tickEvery subscribe
