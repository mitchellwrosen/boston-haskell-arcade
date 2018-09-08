{-# LANGUAGE TemplateHaskell #-}

module Bha.Game.Impl.BlimpBoy
  ( game
  ) where

import Bha.Elm.Prelude

import qualified Data.IntSet as IntSet


--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

type Row = Int
type Col = Int

blimpcol  = 35 :: Col
blimprow  = 5  :: Row
castlecol = 40 :: Col
enemycol  = 0  :: Col
enemyrow  = 20 :: Row

data Model
  = Model
  { _modelEnemiesL :: !IntSet
  , _modelBombL    :: !(Maybe Int)
  , _modelHealthL  :: !Int
  }
makeFields ''Model

init :: Init Model
init = do
  pure Model
    { _modelEnemiesL = mempty
    , _modelBombL    = Nothing
    , _modelHealthL  = 50
    }


--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------

-- TODO Space bar to drop a bomb

update :: Either NominalDiffTime Event -> Update Model ()
update event =
  case event of
    Left _ -> do
      health0 <- use healthL
      guard (health0 > 0)

      enemies0 <- use enemiesL
      bomb0    <- use bombL

      -- March enemies forward by 1
      let
        enemies1 =
          IntSet.map (+1) enemies0

      -- Move bomb down
      let
        bomb1 = do
          bomb <- (+1) <$> bomb0
          guard (bomb <= enemyrow)
          pure bomb

      -- Kill any enemy being bombed
      let
        enemies2 =
          if bomb1 == Just enemyrow
            then IntSet.delete blimpcol enemies1
            else enemies1

      -- Kill any enemy hitting the castle
      let
        enemies3 =
          IntSet.delete castlecol enemies2

      -- Possibly spawn new enemy
      enemies4 <- do
        pct <- randomPct
        if pct > 0.96
          then pure (IntSet.insert enemycol enemies3)
          else pure enemies3

      enemiesL .= enemies4
      bombL    .= bomb1

      -- If an enemy hit the castle, decrease health by 1
      healthL %=
        if IntSet.member castlecol enemies2
          then subtract 1
          else id

    Right (EventKey KeySpace _) ->
      bombL %= \case
        Nothing -> Just blimprow
        Just x  -> Just x

    Right (EventKey KeyEsc _) ->
      empty

    Right _ ->
      pure ()


--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

view :: Model -> Scene
view model =
  Scene cells NoCursor
 where
  cells :: Cells
  cells =
    mconcat
      [ renderSky
      , renderGround
      , renderBlimp
      , renderCastle
      , renderBomb (model ^. bombL)
      , renderEnemies (model ^. enemiesL)
      , renderHealth (model ^. healthL)
      ]

renderSky    = rect 0 0 60 (enemyrow+1) blue
renderGround = rect 0 (enemyrow+1) 60 10 green
renderCastle = rect castlecol 5 10 (enemyrow - 5 + 1) red

renderBlimp :: Cells
renderBlimp =
  rect (blimpcol-1) (blimprow-1) 3 2 yellow

renderBomb :: Maybe Int -> Cells
renderBomb =
  foldMap (\r -> set blimpcol r (Cell '•' black blue))

renderEnemies :: IntSet -> Cells
renderEnemies =
  foldMap (\c -> set c enemyrow (Cell 'o' black blue)) . IntSet.toList

renderHealth :: Int -> Cells
renderHealth health =
  text 0 (enemyrow+11) white black ("Health: " ++ show health)


--------------------------------------------------------------------------------
-- Tick
--------------------------------------------------------------------------------

tickEvery :: Model -> Maybe NominalDiffTime
tickEvery _ =
  Just 0.1


--------------------------------------------------------------------------------
-- Game
--------------------------------------------------------------------------------

game :: ElmGame Model
game =
  ElmGame init update view tickEvery
