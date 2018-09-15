{-# LANGUAGE TemplateHaskell #-}

module Bha.Game.Impl.BlimpBoy
  ( game
  ) where

import Bha.Elm.Prelude

import qualified Data.IntSet as IntSet
import qualified Data.Set as Set


--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

type Row = Int
type Col = Int

blimprow  = 5  :: Row
castlecol = 40 :: Col
enemycol  = 0  :: Col
enemyrow  = 20 :: Row
bombtimer = 5  :: Int

data Model
  = Model
  { _modelBlimpL       :: !Col
  , _modelEnemiesL     :: !IntSet
  , _modelBombL        :: !(Set (Col, Row))
  , _modelNumBombsL    :: !Int
  , _modelMaxNumBombsL :: !Int
  , _modelNextBombL    :: !Int
  , _modelHealthL      :: !Int
  , _modelMoneyL       :: !Int
  } deriving (Show)
makeFields ''Model

init :: Init Void Model
init = do
  pure Model
    { _modelBlimpL       = 35
    , _modelEnemiesL     = mempty
    , _modelBombL        = mempty
    , _modelNumBombsL    = 1
    , _modelMaxNumBombsL = 2
    , _modelNextBombL    = bombtimer
    , _modelHealthL      = 50
    , _modelMoneyL       = 0
    }


--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------

update :: Input Void -> Update Model Void ()
update = \case
  Tick _ ->
    tickUpdate

  Key KeyArrowLeft ->
    blimpL %= max 1 . subtract 1

  Key KeyArrowRight ->
    blimpL %= min 55 . (+ 1)

  Key (KeyChar 'b') -> do
    money       <- use moneyL
    maxNumBombs <- use maxNumBombsL

    when (money >= 1 && maxNumBombs < 5) $ do
      moneyL -= 1
      maxNumBombsL += 1

  Key KeySpace -> do
    supply   <- use numBombsL
    blimpcol <- use blimpL

    bombL %=
      if supply >= 1
        then Set.insert (blimpcol, blimprow)
        else id

    numBombsL %=
      (max 0 . subtract 1)

  Key KeyEsc ->
    empty

  _ ->
    pure ()

tickUpdate :: Update Model Void ()
tickUpdate = do
  use healthL >>= isCastleAlive
  enemiesL %= enemiesAdvance
  bombL %= bombsFallDownward
  use bombL >>= removeBombedEnemies
  use enemiesL >>= enemiesHitCastle
  possiblySpawnNewEnemy
  use nextBombL >>= checkIfNewBombAvailable
  nextBombL %= possiblyResetBombTimer

isCastleAlive :: Int -> Update Model Void ()
isCastleAlive health = do
  guard (health > 0)

enemiesAdvance :: IntSet -> IntSet
enemiesAdvance =
  IntSet.map (+1)

bombsFallDownward :: Ord a => Set (a, Row) -> Set (a, Row)
bombsFallDownward = do
  Set.filter ((<= enemyrow) . snd) . Set.map (over _2 (+1))

removeBombedEnemies :: Set (Row, Col) -> Update Model Void ()
removeBombedEnemies bombs = do
  for_ bombs $ \(bombcol, bombrow) -> do
    enemies <- use enemiesL

    when (bombrow == enemyrow && IntSet.member bombcol enemies) $ do
      moneyL += 1
      enemiesL %= IntSet.delete bombcol

enemiesHitCastle :: IntSet -> Update Model Void ()
enemiesHitCastle enemies = do
  when (IntSet.member castlecol enemies) (healthL -= 1)
  enemiesL %= IntSet.delete castlecol

possiblySpawnNewEnemy :: Update Model Void ()
possiblySpawnNewEnemy = do
  pct <- randomPct
  when (pct > 0.96) (enemiesL %= IntSet.insert enemycol)

checkIfNewBombAvailable :: Int -> Update Model Void ()
checkIfNewBombAvailable nextBombTimer = do
  maxNumBombs <- use maxNumBombsL
  numBombsL %=
    if nextBombTimer == 0
      then min maxNumBombs . (+1)
      else id

possiblyResetBombTimer :: Int -> Int
possiblyResetBombTimer timer =
  if timer == 0
    then bombtimer
    else timer - 1

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
      , renderCastle
      , renderBlimp (model ^. blimpL)
      , renderBombs (model ^. bombL)
      , renderEnemies (model ^. enemiesL)
      , renderMoney (model ^. moneyL)
      , renderHealth (model ^. healthL)
      , renderNumBombs (model ^. numBombsL)
      ]

renderSky    = rect 0 0 60 (enemyrow+1) blue
renderGround = rect 0 (enemyrow+1) 60 10 green
renderCastle = rect castlecol 5 10 (enemyrow - 5 + 1) red

renderBlimp :: Int -> Cells
renderBlimp col =
  rect (col-1) (blimprow-1) 3 2 yellow

renderBombs :: Set (Col, Row) -> Cells
renderBombs =
  foldMap (\(c, r) -> set c r (Cell '•' black blue)) . Set.toList

renderEnemies :: IntSet -> Cells
renderEnemies =
  foldMap (\c -> set c enemyrow (Cell 'o' black blue)) . IntSet.toList

renderMoney :: Int -> Cells
renderMoney money =
  text 0 (enemyrow+11) white black ("Money: " ++ show money)

renderNumBombs :: Int -> Cells
renderNumBombs bombs =
  text 0 (enemyrow+12) white black ("Bombs: " ++ show bombs)

renderHealth :: Int -> Cells
renderHealth health =
  text 0 (enemyrow+13) white black ("Health: " ++ show health)


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
