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
bombtimer = 5  :: Int

data Model
  = Model
  { _modelEnemiesL  :: !IntSet
  , _modelBombL     :: !IntSet
  , _modelNumBombsL :: !Int
  , _modelNextBombL :: !Int
  , _modelHealthL   :: !Int
  } deriving (Show)
makeFields ''Model

init :: Init Void Model
init = do
  pure Model
    { _modelEnemiesL  = mempty
    , _modelBombL     = mempty
    , _modelNumBombsL = 1
    , _modelNextBombL = bombtimer
    , _modelHealthL   = 50
    }


--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------

update :: Input Void -> Update Model Void ()
update = \case
  Tick _ -> do
    health0 <- use healthL
    guard (health0 > 0)

    enemies0 <- use enemiesL
    bombs0   <- use bombL

    -- March enemies forward by 1
    let
      enemies1 =
        IntSet.map (+1) enemies0

    -- Move bombs down
    let
      bombs1 =
        IntSet.filter (<= enemyrow) (IntSet.map (+1) bombs0)

    -- Kill any enemy being bombed
    let
      enemies2 =
        if IntSet.member enemyrow bombs1
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

    timer <- use nextBombL

    numBombsL %=
      if timer == 0
        then min 3 . (+1)
        else id

    nextBombL .=
      if timer == 0
        then bombtimer
        else timer - 1

    enemiesL .= enemies4
    bombL    .= bombs1

    -- If an enemy hit the castle, decrease health by 1
    healthL %=
      if IntSet.member castlecol enemies2
        then subtract 1
        else id

  Key KeySpace -> do
    supply <- use numBombsL

    bombL %=
      if supply >= 1
        then IntSet.insert blimprow
        else id

    numBombsL %=
      (max 0 . subtract 1)

  Key KeyEsc ->
    empty

  _ ->
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
      , renderBombs (model ^. bombL)
      , renderEnemies (model ^. enemiesL)
      , renderHealth (model ^. healthL)
      , renderNumBombs (model ^. numBombsL)
      ]

renderSky    = rect 0 0 60 (enemyrow+1) blue
renderGround = rect 0 (enemyrow+1) 60 10 green
renderCastle = rect castlecol 5 10 (enemyrow - 5 + 1) red

renderBlimp :: Cells
renderBlimp =
  rect (blimpcol-1) (blimprow-1) 3 2 yellow

renderBombs :: IntSet -> Cells
renderBombs =
  foldMap (\r -> set blimpcol r (Cell '•' black blue)) . IntSet.toList

renderEnemies :: IntSet -> Cells
renderEnemies =
  foldMap (\c -> set c enemyrow (Cell 'o' black blue)) . IntSet.toList

renderHealth :: Int -> Cells
renderHealth health =
  text 0 (enemyrow+11) white black ("Health: " ++ show health)

renderNumBombs :: Int -> Cells
renderNumBombs bombs =
  text 0 (enemyrow+12) white black ("Bombs: " ++ show bombs)


--------------------------------------------------------------------------------
-- Tick
--------------------------------------------------------------------------------

tickEvery :: Model -> Maybe NominalDiffTime
tickEvery _ =
  Just 0.1


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
