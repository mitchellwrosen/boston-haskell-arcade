{-# LANGUAGE TemplateHaskell #-}

module Bha.Game.Impl.BlimpBoy
  ( game
  ) where

import Bha.Elm.Prelude

import qualified Data.Set as Set


-- TODO Blimp boy - enemies that fire upwards
-- TODO Blimp boy - enemy blimps that fire downwards
-- TODO Blimp boy - levels

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

blimpx      = 35  :: X
blimprow    = 5   :: Row
blimpvel    = 3   :: Vel
castlecol   = 40  :: Col
enemycol    = 0   :: X
enemyrow    = 20  :: Row
enemyvel    = 7   :: Vel
pebblevel   = 24  :: Vel
pebbletimer = 1.0 :: Seconds
bombvel     = 18  :: Vel
bombtimer   = 3.0 :: Seconds

type Row = Int
type Col = Int
type X   = Double
type Y   = Double
type Vel = Double

data Model
  = Model
  { _modelBlimpL         :: !X
  , _modelBlimpVelL      :: !Vel
  , _modelEnemiesL       :: !(Set X)
  , _modelPebbleL        :: !(Set (X, Y))
  , _modelNumPebblesL    :: !Int
  , _modelMaxNumPebblesL :: !Int
  , _modelNextPebbleL    :: !Seconds
  , _modelBombsL         :: !(Set (X, Y))
  , _modelNumBombsL      :: !Int
  , _modelMaxNumBombsL   :: !Int
  , _modelNextBombL      :: !Seconds
  , _modelHealthL        :: !Int
  , _modelMoneyL         :: !Int
  } deriving (Show)
makeFields ''Model

init :: Init Void Model
init = do
  pure Model
    { _modelBlimpL         = blimpx
    , _modelBlimpVelL      = -blimpvel
    , _modelEnemiesL       = mempty
    , _modelPebbleL        = mempty
    , _modelNumPebblesL    = 1
    , _modelMaxNumPebblesL = 2
    , _modelNextPebbleL    = pebbletimer
    , _modelBombsL         = mempty
    , _modelNumBombsL      = 1
    , _modelMaxNumBombsL   = 2
    , _modelNextBombL      = bombtimer
    , _modelHealthL        = 50
    , _modelMoneyL         = 0
    }


--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------

update :: Input Void -> Update Model Void ()
update = \case
  Tick dt ->
    tickUpdate dt

  Key KeyArrowLeft ->
    blimpVelL %= negate . abs

  Key KeyArrowRight ->
    blimpVelL %= abs

  Key (KeyChar 'p') -> do
    money         <- use moneyL
    maxNumPebbles <- use maxNumPebblesL

    when (money >= 1 && maxNumPebbles < 5) $ do
      moneyL %= subtract 1
      maxNumPebblesL %= (+1)

  Key (KeyChar 'b') -> do
    money       <- use moneyL
    maxNumBombs <- use maxNumBombsL

    when (money >= 3 && maxNumBombs < 5) $ do
      moneyL %= subtract 3
      maxNumBombsL %= (+1)

  Key KeySpace -> do
    supply   <- use numPebblesL
    blimpcol <- use blimpL

    pebbleL %=
      if supply >= 1
        then
          Set.insert (blimpcol + 0.5, fromIntegral blimprow + 0.5)
        else
            id

    numPebblesL %=
      (max 0 . subtract 1)

  Key (KeyChar '1') -> do
    supply   <- use numBombsL
    blimpcol <- use blimpL

    bombsL %=
      if supply >= 1
        then
          Set.insert (blimpcol + 0.5, fromIntegral blimprow + 0.5)
        else
          id

    numBombsL %=
      (max 0 . subtract 1)

  Key KeyEsc ->
    empty

  _ ->
    pure ()

tickUpdate :: Seconds -> Update Model Void ()
tickUpdate dt = do
  isCastleAlive

  blimpDrifts dt
  enemiesAdvance dt
  stuffFallsDownward dt

  removePebbledEnemies
  removeBombedEnemies
  enemiesHitCastle
  possiblySpawnNewEnemy
  updatePebbleSupply dt
  updateBombSupply dt

isCastleAlive :: Update Model Void ()
isCastleAlive = do
  health <- use healthL
  guard (health > 0)

blimpDrifts :: Seconds -> Update Model Void ()
blimpDrifts dt = do
  blimpVel <- use blimpVelL
  blimpL %= max 1 . min 55 . \x -> x + blimpVel * realToFrac dt

enemiesAdvance :: Seconds -> Update Model Void ()
enemiesAdvance dt =
  enemiesL %= Set.map (\x -> x + enemyvel * realToFrac dt)

stuffFallsDownward :: Seconds -> Update Model Void ()
stuffFallsDownward dt = do
  pebbleL %=
    Set.filter ((\y -> y <= fromIntegral enemyrow + 1) . snd) .
      Set.map (over _2 (\y -> y + pebblevel * realToFrac dt))

  bombsL %=
    Set.filter ((\y -> y <= fromIntegral enemyrow + 1) . snd) .
      Set.map (over _2 (\y -> y + bombvel * realToFrac dt))

removePebbledEnemies :: Update Model Void ()
removePebbledEnemies = do
  pebbles <- use pebbleL

  for_ pebbles $ \(pebblex, pebbley) -> do
    when (floor pebbley == enemyrow) $ do
      enemies <- use enemiesL

      let
        (dead, alive) =
          Set.partition
            (\x -> x >= pebblex - 0.5 && x <= pebblex + 0.5)
            enemies

      moneyL %= (+ length dead)
      enemiesL .= alive

removeBombedEnemies :: Update Model Void ()
removeBombedEnemies = do
  bombs <- use bombsL

  for_ bombs $ \(bombx, bomby) -> do
    when (floor bomby == enemyrow) $ do
      enemies <- use enemiesL

      let
        (dead, alive) =
          Set.partition (\x -> x >= bombx - 1.5 && x <= bombx + 1.5) enemies

      enemiesL .= alive
      moneyL %= (+ length dead)

enemiesHitCastle :: Update Model Void ()
enemiesHitCastle = do
  enemies <- use enemiesL

  let
    (splat, walking) =
      Set.partition (\x -> floor x >= castlecol) enemies

  enemiesL .= walking
  healthL %= subtract (length splat)

possiblySpawnNewEnemy :: Update Model Void ()
possiblySpawnNewEnemy = do
  pct <- randomPct
  when (pct > 0.99) (enemiesL %= Set.insert enemycol)

updatePebbleSupply :: Seconds -> Update Model Void ()
updatePebbleSupply dt = do
  nextPebbleTimer <- use nextPebbleL
  maxNumPebbles   <- use maxNumPebblesL
  numPebblesL %=
    if nextPebbleTimer <= 0
      then min maxNumPebbles . (+1)
      else id
  nextPebbleL %=
    \timer ->
      if timer <= 0
        then timer - dt + pebbletimer
        else timer - dt

updateBombSupply :: Seconds -> Update Model Void ()
updateBombSupply dt = do
  nextBombTimer <- use nextBombL
  maxNumBombs   <- use maxNumBombsL
  numBombsL %=
    if nextBombTimer <= 0
      then min maxNumBombs . (+1)
      else id
  nextBombL %=
    \timer ->
      if timer <= 0
        then timer - dt + bombtimer
        else timer - dt

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
      , renderPebbles (model ^. pebbleL)
      , renderBombs (model ^. bombsL)
      , renderEnemies (model ^. enemiesL)
      , renderMoney (model ^. moneyL)
      , renderNumPebbles (model ^. numPebblesL)
      , renderNumBombs (model ^. numBombsL)
      , renderHealth (model ^. healthL)
      ]

renderSky    = rect 0 0 60 (enemyrow+1) blue
renderGround = rect 0 (enemyrow+1) 60 10 green
renderCastle = rect castlecol 5 10 (enemyrow - 5 + 1) 253

renderBlimp :: X -> Cells
renderBlimp (round -> col) =
  rect (col-1) (blimprow-1) 3 2 yellow

renderPebbles :: Set (X, Y) -> Cells
renderPebbles =
  foldMap (\(floor -> c, floor -> r) -> set c r (Cell '‧' black blue)) .
    Set.toList

renderBombs :: Set (X, Y) -> Cells
renderBombs =
  foldMap render . Set.toList
 where
  render :: (X, Y) -> Cells
  render (floor -> c, floor -> r) =
    if r == enemyrow
      then
        rect (c-1) r 3 1 red
      else
        set c r (Cell '•' black blue)

renderEnemies :: Set X -> Cells
renderEnemies =
  foldMap (\c -> set (round c) enemyrow (Cell '∞' black blue)) . Set.toList

renderMoney :: Int -> Cells
renderMoney money =
  text 0 (enemyrow+11) white black ("Money: " ++ show money)

renderNumPebbles :: Int -> Cells
renderNumPebbles pebbles =
  text 0 (enemyrow+12) white black ("Pebbles: " ++ replicate pebbles '‧')

renderNumBombs :: Int -> Cells
renderNumBombs bombs =
  text 0 (enemyrow+13) white black ("Bombs: " ++ replicate bombs '•')

renderHealth :: Int -> Cells
renderHealth health =
  text 0 (enemyrow+14) white black ("Health: " ++ show health)


--------------------------------------------------------------------------------
-- Tick
--------------------------------------------------------------------------------

tickEvery :: Model -> Maybe Seconds
tickEvery _ =
  Just (1/30)


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
