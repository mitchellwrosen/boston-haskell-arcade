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
  { blimp         :: X
  , blimpVel      :: Vel
  , enemies       :: (Set X)
  , pebble        :: (Set (X, Y))
  , numPebbles    :: Int
  , maxNumPebbles :: Int
  , nextPebble    :: Seconds
  , bombs         :: (Set (X, Y))
  , numBombs      :: Int
  , maxNumBombs   :: Int
  , nextBomb      :: Seconds
  , health        :: Int
  , money         :: Int
  } deriving stock (Generic, Show)

init :: Init Void Model
init = do
  pure Model
    { blimp         = blimpx
    , blimpVel      = -blimpvel
    , enemies       = mempty
    , pebble        = mempty
    , numPebbles    = 1
    , maxNumPebbles = 2
    , nextPebble    = pebbletimer
    , bombs         = mempty
    , numBombs      = 1
    , maxNumBombs   = 2
    , nextBomb      = bombtimer
    , health        = 50
    , money         = 0
    }


--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------

update :: Input Void -> Update Model Void ()
update = \case
  Tick dt ->
    tickUpdate dt

  Key KeyArrowLeft ->
    #blimpVel %= negate . abs

  Key KeyArrowRight ->
    #blimpVel %= abs

  Key (KeyChar 'p') -> do
    money         <- use #money
    maxNumPebbles <- use #maxNumPebbles

    when (money >= 1 && maxNumPebbles < 5) $ do
      #money %= subtract 1
      #maxNumPebbles %= (+1)

  Key (KeyChar 'b') -> do
    money       <- use #money
    maxNumBombs <- use #maxNumBombs

    when (money >= 3 && maxNumBombs < 5) $ do
      #money %= subtract 3
      #maxNumBombs %= (+1)

  Key KeySpace -> do
    supply   <- use #numPebbles
    blimpcol <- use #blimp

    #pebble %=
      if supply >= 1
        then
          Set.insert (blimpcol + 0.5, fromIntegral blimprow + 0.5)
        else
            id

    #numPebbles %=
      (max 0 . subtract 1)

  Key (KeyChar '1') -> do
    supply   <- use #numBombs
    blimpcol <- use #blimp

    #bombs %=
      if supply >= 1
        then
          Set.insert (blimpcol + 0.5, fromIntegral blimprow + 0.5)
        else
          id

    #numBombs %=
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
  health <- use #health
  guard (health > 0)

blimpDrifts :: Seconds -> Update Model Void ()
blimpDrifts dt = do
  blimpVel <- use #blimpVel
  #blimp %= max 1 . min 55 . \x -> x + blimpVel * realToFrac dt

enemiesAdvance :: Seconds -> Update Model Void ()
enemiesAdvance dt =
  #enemies %= Set.map (\x -> x + enemyvel * realToFrac dt)

stuffFallsDownward :: Seconds -> Update Model Void ()
stuffFallsDownward dt = do
  #pebble %=
    Set.filter ((\y -> y <= fromIntegral enemyrow + 1) . snd) .
      Set.map (over _2 (\y -> y + pebblevel * realToFrac dt))

  #bombs %=
    Set.filter ((\y -> y <= fromIntegral enemyrow + 1) . snd) .
      Set.map (over _2 (\y -> y + bombvel * realToFrac dt))

removePebbledEnemies :: Update Model Void ()
removePebbledEnemies = do
  pebbles <- use #pebble

  for_ pebbles $ \(pebblex, pebbley) -> do
    when (floor pebbley == enemyrow) $ do
      enemies <- use #enemies

      let
        (dead, alive) =
          Set.partition
            (\x -> x >= pebblex - 0.5 && x <= pebblex + 0.5)
            enemies

      #money %= (+ length dead)
      #enemies .= alive

removeBombedEnemies :: Update Model Void ()
removeBombedEnemies = do
  bombs <- use #bombs

  for_ bombs $ \(bombx, bomby) -> do
    when (floor bomby == enemyrow) $ do
      enemies <- use #enemies

      let
        (dead, alive) =
          Set.partition (\x -> x >= bombx - 1.5 && x <= bombx + 1.5) enemies

      #enemies .= alive
      #money %= (+ length dead)

enemiesHitCastle :: Update Model Void ()
enemiesHitCastle = do
  enemies <- use #enemies

  let
    (splat, walking) =
      Set.partition (\x -> floor x >= castlecol) enemies

  #enemies .= walking
  #health %= subtract (length splat)

possiblySpawnNewEnemy :: Update Model Void ()
possiblySpawnNewEnemy = do
  pct <- randomPct
  when (pct > 0.99) (#enemies %= Set.insert enemycol)

updatePebbleSupply :: Seconds -> Update Model Void ()
updatePebbleSupply dt = do
  nextPebbleTimer <- use #nextPebble
  maxNumPebbles   <- use #maxNumPebbles

  #numPebbles %=
    if nextPebbleTimer <= 0
      then min maxNumPebbles . (+1)
      else id

  #nextPebble %=
    \timer ->
      if timer <= 0
        then timer - dt + pebbletimer
        else timer - dt

updateBombSupply :: Seconds -> Update Model Void ()
updateBombSupply dt = do
  nextBombTimer <- use #nextBomb
  maxNumBombs   <- use #maxNumBombs

  #numBombs %=
    if nextBombTimer <= 0
      then min maxNumBombs . (+1)
      else id

  #nextBomb %=
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
      , renderBlimp (model ^. #blimp)
      , renderPebbles (model ^. #pebble)
      , renderBombs (model ^. #bombs)
      , renderEnemies (model ^. #enemies)
      , renderMoney (model ^. #money)
      , renderNumPebbles (model ^. #numPebbles)
      , renderNumBombs (model ^. #numBombs)
      , renderHealth (model ^. #health)
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
