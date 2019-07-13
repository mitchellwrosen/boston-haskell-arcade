module Bha.Game.Impl.Pong
  ( game
  ) where

import Bha.Elm.Prelude

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

setPaddleSize:: Int
gameSpeed :: Seconds

setPaddleSize = 3
gameSpeed = 15 

type Pos = Int
type Vel = Int
type Score = Int


data Model
  = Model
  { padSize    :: Int 
  , myPadPos   :: Pos
  , myPadVel   :: Vel 
  , opPadPos   :: Pos
  , opPadVel   :: Vel
  , ballXPos   :: Pos
  , ballYPos   :: Pos
  , ballXVel   :: Vel
  , ballYVel   :: Vel
  , myScore    :: Score
  , opScore    :: Score
  , myScorePos :: Pos
  , opScorePos :: Pos
  , columns    :: Pos
  , rows       :: Pos
  , opPadCol   :: Pos 
  , botBorder  :: Pos
  , wait       :: Int
  } deriving (Show, Generic)


init :: Int -> Int -> Init Void Model
init width height = do

  pure Model
    { padSize     = setPaddleSize
    , myPadPos    = height `div` 2
    , myPadVel    = 0
    , opPadPos    = height `div` 2
    , opPadVel    = 0
    , ballXPos    = 30
    , ballYPos    = 15
    , ballXVel    = -1
    , ballYVel    = -1
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
            #myPadPos %= min (rows - 3) . (+1)

          Key KeyArrowUp -> do 
            #opPadPos %= max 0 . subtract 1

          Key KeyArrowDown -> do 
            rows <- use #rows 
            #opPadPos %= min (rows - 3) . (+1)

          _ ->
            pure ()

updateTick :: Update Model Void ()
updateTick = do
  model :: Model <- get

  let
    ballXPos = (model ^. #ballXPos)
    ballYPos = (model ^. #ballYPos)
    ballNextX = (model ^. #ballXPos) + (model ^. #ballXVel)  
    ballNextY = (model ^. #ballYPos) + (model ^. #ballYVel)
    ballXVel = (model ^. #ballXVel)
    ballYVel = (model ^. #ballYVel)
    myPadPos = (model ^. #myPadPos)
    opPadPos = (model ^. #opPadPos)


  if
     | ballNextX < 0 ->
       #opScore .= (+1) (model ^. #opScore) 
       -- reset function
     | ballNextX >= (model ^. #columns) ->
       #myScore .= (+1) (model ^. #opScore) 
     
     | ballNextX == 0 && myPadPos <= ballNextY && ballNextY <= myPadPos + 2 -> do
        #ballXVel .= negate ballXVel
        #ballXPos .= ballXPos + 1 
        #ballYPos .= ballYPos + ballYVel

  {- Line 106 error
     | ballNextX == opPadCol && opPadPos <= ballNextY && ballNextY <= opPadPos + 2 -> do
        #ballXVel .= negate ballXVel
        #ballXPos .= ballXPos - 1
        #ballYPos .= ballYPos + ballYVel 
        --}
        

     | ballXPos == (model ^. #columns) ->
       #myScore .= (+1) (model ^. #myScore)
       -- reset function

     | ballNextY == 0 -> do
       #ballYVel .= negate ballYVel
       #ballXPos .= ballXPos + ballXVel
       #ballYPos .= ballYPos + 1


     | ballYPos == (model ^. #botBorder) -> do
       #ballYVel .= negate ballYVel
       #ballXPos .= ballXPos + ballXVel
       #ballYPos .= ballYPos - 1

     | otherwise -> do
       #ballXPos .= ballXPos + ballXVel
       #ballYPos .= ballYPos + ballYVel

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
      -- , rect (model ^. (fmap (\x -> x - 1) #columns)) (model ^. #opPaddlePos) 1 3 white -- renderOpPaddle 
      , text 1 0 mempty mempty (show (model ^. #myPadPos))
      , text 4 0 mempty mempty (show (model ^. #ballXPos))
      , text 7 0 mempty mempty (show (model ^. #ballYPos))
      -- , text 9 0 mempty mempty (show (model ^. #ballYPos) +~ 1)
      , set (model ^. #ballXPos) (model ^. #ballYPos) (Cell 'O' mempty mempty)
      , rect (model ^. #opPadCol) (model ^. #opPadPos) 1 3 white
      , rect 0 (model ^. #myPadPos) 1 3 white
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
