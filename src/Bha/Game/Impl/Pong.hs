module Bha.Game.Impl.Pong
  ( game
  ) where

import Bha.Elm.Prelude

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

-- setPaddleSize:: Int
-- setPaddleSize = 3 

gameSpeed :: Seconds

gameSpeed = 10 

type XPos = Int
type YPos = Int
type XVel = Int
type YVel = Int
type Score = Int

type Ball = (XPos, YPos)
type BallVec = (XVel, YVel)
type Paddle = (XPos, YPos)

getRandomVel :: MonadElm message m => m Int
getRandomVel = randomInt 0 1 >>= \case
  0 -> pure (-1)
  1 -> pure 1
  
randomVector :: MonadElm message m => m (Int,Int)
randomVector =
  (,) <$> getRandomVel <*> getRandomVel

data Model
  = Model
  { padSize    :: Int 
  , myPadPos   :: YPos
  , opPadPos   :: YPos
  , ball       :: Ball
  , ballvec    :: BallVec
  , ballreset  :: Ball
  , myScore    :: Score
  , opScore    :: Score
  , myScorePos :: XPos
  , opScorePos :: XPos
  , columns    :: Int
  , rows       :: Int
  , opPadCol   :: XPos 
  , botBorder  :: YPos
  , wait       :: Int
  } deriving (Show, Generic)


init :: Int -> Int -> Init Void Model
init width height = do
  randvec <- randomVector
  let 
    xMid = (width `div` 2)
    yMid = (height `div` 2)
    setPaddleSize = height `div` 7 
  

  pure Model
    { padSize     = setPaddleSize
    , myPadPos    = yMid
    , opPadPos    = yMid
    , ball        = (xMid,yMid) 
    , ballvec     = randvec 
    , ballreset   = (xMid,yMid)
    , myScore     = 0
    , opScore     = 0
    , myScorePos  = width `div` 4
    , opScorePos  = 3 * (width `div` 4)
    , columns     = width
    , rows        = height
    , opPadCol    = width - 1 
    , botBorder   = height - 1
    , wait        = 10
    }


--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------

update :: Input Void -> Update Model Void ()
update = \case

          Tick _ ->
            updateTick

          Key KeyEsc ->
            gameover

          Key (KeyChar 'w') -> do 
            #myPadPos %= max 0 . subtract 1

          Key (KeyChar 's') -> do
            rows <- use #rows 
            padsize <- use #padSize
            #myPadPos %= min (rows - padsize) . (+1)

          Key KeyArrowUp -> do 
            #opPadPos %= max 0 . subtract 1

          Key KeyArrowDown -> do 
            rows <- use #rows 
            padsize <- use #padSize
            #opPadPos %= min (rows - padsize) . (+1)

          _ ->
            pure ()

updateTick :: Update Model Void ()
updateTick = do
  model :: Model <- get

  let
    ballNextX = ballx + ballxvel  
    ballNextY = bally + ballyvel
    myPadPos = (model ^. #myPadPos)
    opPadPos = (model ^. #opPadPos)
    opPadCol = (model ^. #opPadCol)
    (ballx, bally) = (model ^. #ball)
    (ballxvel, ballyvel) = (model ^. #ballvec)
    padsize = (model ^. #padSize)


  if
     | ballNextX < 0 -> do
       newrandvec <- randomVector
       #opScore .= (+1) (model ^. #opScore)
       #ball .= (model ^. #ballreset)
       #ballvec .= newrandvec

     | ballNextX >= (model ^. #columns) -> do
       newrandvec <- randomVector
       #myScore .= (+1) (model ^. #myScore) 
       #ball .= (model ^. #ballreset)
       #ballvec .= newrandvec
     
     | ballNextX == 0 && myPadPos <= ballNextY && ballNextY <= myPadPos + padsize -> do
        #ballvec .= (negate ballxvel, ballyvel)
        #ball .= (ballx + negate ballxvel, bally + ballyvel)

     | ballNextX == opPadCol && opPadPos <= ballNextY && ballNextY <= opPadPos + padsize -> do
        #ballvec .= (negate ballxvel, ballyvel)
        #ball .= (ballx + negate ballxvel, bally + ballyvel)

     | ballNextY <= 0 -> do
       #ballvec .= (ballxvel, 1)
       #ball .= (ballx + ballxvel, bally + 1)

     | ballNextY >= (model ^. #rows) -> do
       #ballvec .= (ballxvel, (-1))
       #ball .= (ballx + ballxvel, bally - 1)

     | otherwise -> do
       #ball .= (ballx + ballxvel, bally + ballyvel)

-- move :: 
-- move = do
-- reset = do

--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

view :: Model -> Scene
view model =
  Scene cells NoCursor
 where
  cells :: Cells
  cells = -- rect 0 1 1 3 white {-
    mconcat
      [ text (model ^. #myScorePos) 0 mempty mempty (show (model ^. #myScore))
      , text (model ^. #opScorePos) 0 mempty mempty (show (model ^. #opScore))
      , rect ((model ^. #columns) - 1) (model ^. #opPadPos) 1 (model ^. #padSize) white 
      -- , text 4 0 mempty mempty (show (model ^. #ball))
      , set (fst (model ^. #ball)) (snd (model ^. #ball)) (Cell 'O' mempty mempty)
      , rect 0 (model ^. #myPadPos) 1 (model ^. #padSize) white
      ]  


--------------------------------------------------------------------------------
-- Tick
--------------------------------------------------------------------------------

tickEvery :: Model -> Maybe Seconds
tickEvery model = Just (1/gameSpeed)


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
