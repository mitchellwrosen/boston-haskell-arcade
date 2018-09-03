-- | Example Elm-style game.

{-# LANGUAGE FunctionalDependencies, LambdaCase, MultiParamTypeClasses,
             NamedFieldPuns, NoImplicitPrelude, RecordWildCards,
             TemplateHaskell #-}

module Bha.Game.Impl.ElmExample
  ( game
  ) where

import Bha.Elm.Prelude


--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

data Model
  = Model
  { _modelCountL :: !Int
  , _modelElapsedL :: !NominalDiffTime
  }
makeFields ''Model

init :: Init Model
init =
  pure Model
    { _modelCountL = 0
    , _modelElapsedL = 0
    }


--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------

update :: Either NominalDiffTime Event -> Update Model ()
update = \case
  Right (EventKey KeyEsc _) ->
    empty

  Right _ -> do
    n <- use countL
    guard (n < 10)
    countL .= (n+1)

  Left delta ->
    elapsedL %= (+ delta)


--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

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


--------------------------------------------------------------------------------
-- Tick
--------------------------------------------------------------------------------

-- Tick once per second.
tickEvery :: Model -> Maybe NominalDiffTime
tickEvery _ =
  Just 1

--------------------------------------------------------------------------------
-- Game
--------------------------------------------------------------------------------

game :: ElmGame Model
game =
  ElmGame init update view tickEvery
