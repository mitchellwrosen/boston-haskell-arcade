module Bha.Game.Impl.Pong
  ( game
  ) where

import Bha.Elm.Prelude

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

type Pos = Int
type Vel = Int

data Model
  = Model
  { myPaddlePos :: Pos
  , myPaddleVel :: Vel 
  , opPaddlePos :: Pos
  , opPaddleVel :: Vel
  , ballXPos    :: Pos
  , ballYPos    :: Pos
  , ballXVel    :: Vel
  , ballYVel    :: Vel
  , myScore     :: Int
  , opScore     :: Int
  , columns     :: Int
  , rows        :: Int
  , wait        :: Int
  } deriving (Show, Generic)


init :: Int -> Int -> Init Void Model
init width height = do

  pure Model
    { myPaddlePos = height `div` 2
    , myPaddleVel = 0
    , opPaddlePos = height `div` 2
    , opPaddleVel = 0
    , ballXPos    = 30
    , ballYPos    = 15
    , ballXVel    = 1
    , ballYVel    = 1
    , myScore     = 0
    , opScore     = 0
    , columns     = width
    , rows        = height
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

          Key KeyArrowUp -> do 
            #myPaddlePos %= max 0 . subtract 1
            -- TODO: myPaddlePos -1

          Key KeyArrowDown -> do 
            rows <- use #rows 
            #myPaddlePos %= min (rows - 3) . (+1)
            -- TODO: myPaddlePos +1

          _ ->
            pure ()

updateTick :: Update Model Void ()
updateTick = do
  model :: Model <- get

  let
    ballXPos = (model ^. #ballXPos) 
    ballYPos = (model ^. #ballYPos) + (model ^. #ballYVel)
    ballXVel = (model ^. #ballXVel)
    ballYVel = (model ^. #ballYVel)
  if
     | ballXPos == 0 ->
       #myScore .= (model ^. #myScore . (+1)) 

     | ballXPos == (model ^. #rows) ->
       #opScore .= (model ^. #opScore . (+1))

     | ballYPos == 0 ->
       #ballYVel .= negate (model ^. #YVel)

    | ballYPos == 

     | otherwise ->
       #ballXPos .= ballXPos + ballXVel
       #ballYPos .= ballYPos + ballYVel



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
      [ rect 0 (model ^. #myPaddlePos) 1 3 white -- renderMyPaddle 
      , rect 80 3 1 3 white -- renderOpPaddle 
      , set (model ^. #ballXPos) (model ^. #ballYPos) (Cell 'O' mempty mempty)
      ]  
        {-, renderMyScore
      , renderOpScore
      ]

renderMyPaddle = rect 0 10 
--}


--------------------------------------------------------------------------------
-- Tick
--------------------------------------------------------------------------------

tickEvery :: Model -> Maybe Seconds
tickEvery model = Just (1/3)





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
