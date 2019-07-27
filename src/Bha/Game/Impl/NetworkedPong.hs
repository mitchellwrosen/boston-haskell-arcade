module Bha.Game.Impl.NetworkedPong
  ( game
  ) where

import Bha.Elm.Prelude
import qualified Data.HashSet as HashSet

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

-- setPaddleSize:: Int
-- setPaddleSize = 3 

gameSpeed :: Seconds

gameSpeed = 30 



type XPos = Int
type YPos = Int
type XVel = Int
type YVel = Int
type Score = Int

type Ball = (XPos, YPos)
type BallVec = (XVel, YVel)

data Direction = Up | Down
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data GameState = Hosting | Joined | NotPlaying
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

    {-
data Request
    -- TODO(exw): refactor into hello | player1 player2 message types
    = Quit 
    | RequestMove Direction 
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  --}

data Response  
    = BeginPlaying Int Int BallVec 
    | Move1 YPos 
    | Move2 YPos 
    | SendTick Ball Score Score
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Greetings
    = Hello 
    | Quit
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ServerMsg = RequestMove Direction | Sent Response | Echo Greetings
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

getRandomVel :: MonadElm message m => m Int
getRandomVel = randomBool >>= \case
  False -> pure (-1)
  True  -> pure 1

randomVector :: MonadElm message m => m (Int,Int)
randomVector =
  (,) <$> getRandomVel <*> getRandomVel

data Model 
  = Model
  { padSize     :: Int 
  , leftPadPos  :: YPos
  , rightPadPos :: YPos
  , ball        :: Ball
  , ballvec     :: BallVec
  , ballreset   :: Ball
  , myScore     :: Score
  , opScore     :: Score
  , myScorePos  :: XPos
  , opScorePos  :: XPos
  , columns     :: Int
  , rows        :: Int
  , opPadCol    :: XPos 
  , botBorder   :: YPos
  , wait        :: Int
  , gameState   :: GameState
  } deriving (Show, Generic)


init :: Int -> Int -> Init ServerMsg Model
init _ _ = do
  send "Pong" (Echo Hello)
  randvec <- randomVector
  let 
    playingStatus = NotPlaying
    height :: YPos
    height = 25
    width :: XPos
    width = 80
    xMid = (width `div` 2)
    yMid = (height `div` 2)
    setPaddleSize = height `div` 7 
  

  pure Model
    { padSize     = setPaddleSize
    , leftPadPos  = yMid
    , rightPadPos = yMid
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
    , gameState   = playingStatus
    }


--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------

update :: Input ServerMsg -> Update Model ServerMsg ()
update = \case

  Message _ (Echo greet) -> do
    case greet of
      Hello -> do
      --TODO [exw]: race condition if both connect simultaneously
        model <- get
        #gameState .= Hosting
        send "Pong" (Sent (BeginPlaying (model ^. #columns) (model ^. #rows) (model ^. #ballvec)))
      Quit -> do
        gameover

  Message _ (RequestMove dir) -> do
    gamestate <- use #gameState
    case gamestate of
      Hosting -> do
        case dir of
          Up -> #rightPadPos %= max 0 . subtract 1
          Down -> do 
            rows <- use #rows 
            padsize <- use #padSize
            #rightPadPos %= min (rows - padsize) . (+1)
            rightPadPos <- use #rightPadPos
            send "Pong" (Sent (Move2 rightPadPos))

      Joined -> 
        error "Must be host to receive RequestMove"

      NotPlaying -> 
        error "Must be host to receive RequestMove"

  Message _ (Sent res) -> do
    gamestate <- use #gameState
    case gamestate of
      Joined -> do
        case res of
          Move1 ypos -> do 
            #leftPadPos .= ypos

          Move2 ypos -> do
            #rightPadPos .= ypos

          SendTick ball leftScore rightScore -> do
            #ball .= ball
            #myScore .= leftScore
            #opScore .= rightScore

          _ -> do
            error "Invalid Response type"

      Hosting -> 
        error "Must be player 2 to receive Response of type " -- ++ show res

      NotPlaying -> do
        case res of
          BeginPlaying col row initballvec -> do
            #gameState .= Joined
            #columns .= col
            #rows .= row
            #ballvec .= initballvec

          _ ->
            error "Must be playing to receive Response of type " -- ++ show res

  Key KeyEsc -> do
    send "Pong" (Echo Quit)
    gameover

  Key KeyArrowUp -> do 
    -- #rightPadPos %= max 0 . subtract 1
    gamestate <- use #gameState
    case gamestate of
      Hosting -> do
        #leftPadPos %= max 0 . subtract 1
        leftPadPos <- use #leftPadPos
        send "Pong" (Sent (Move1 leftPadPos))
      Joined -> do
        send "Pong" (RequestMove Up)
      NotPlaying -> pure ()

  Key KeyArrowDown -> do 
    -- #rightPadPos %= max 0 . subtract 1
    gamestate <- use #gameState
    case gamestate of
      Hosting -> do
        rows <- use #rows 
        padsize <- use #padSize
        #leftPadPos %= min (rows - padsize) . (+1)
        leftPadPos <- use #leftPadPos
        send "Pong" (Sent (Move1 leftPadPos))
      Joined -> do
        send "Pong" (RequestMove Down)
      NotPlaying -> pure ()

  Tick _ -> do 
    updateTick
    ball <- use #ball
    leftScore <- use #myScore
    rightScore <- use #opScore
    send "Pong" (Sent (SendTick ball leftScore rightScore)) 

  _ ->
    pure ()

updateTick :: Update Model ServerMsg ()
updateTick = do
  model :: Model <- get

  let
    ballNextX = ballx + ballxvel  
    ballNextY = bally + ballyvel
    leftPadPos = (model ^. #leftPadPos)
    rightPadPos = (model ^. #rightPadPos)
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
     
     | ballNextX == 0 && leftPadPos <= ballNextY && ballNextY <= leftPadPos + padsize -> do
        #ballvec .= (negate ballxvel, ballyvel)
        #ball .= (ballx + negate ballxvel, bally + ballyvel)

     | ballNextX == opPadCol && rightPadPos <= ballNextY && ballNextY <= rightPadPos + padsize -> do
        #ballvec .= (negate ballxvel, ballyvel)
        #ball .= (ballx + negate ballxvel, bally + ballyvel)

     | ballNextY <= 0 -> do
       #ballvec .= (ballxvel, 1)
       #ball .= (ballx + ballxvel, bally + 1)

    -- TODO(exw): fix bounce barrier
     | ballNextY >= (model ^. #rows)-1 -> do
       #ballvec .= (ballxvel, (-1))
       #ball .= (ballx + ballxvel, bally - 1)

     | otherwise -> do
       #ball .= (ballx + ballxvel, bally + ballyvel)
       
         {--TODO [exw]: refactor move function in UpdateTick
move :: Ball -> BallVec -> Update Model ServerMsg ()
move ball ballvec = do
  #ballvec .= ballvec
  #ball .= ball
  --}
-- reset = do


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
      [ rect ((model ^. #columns) - 1) (model ^. #rightPadPos) 1 (model ^. #padSize) white 
      , set (fst (model ^. #ball)) (snd (model ^. #ball)) (Cell 'O' mempty mempty)
      , rect 0 (model ^. #leftPadPos) 1 (model ^. #padSize) white
      , rect 0 0 (model ^. #columns) 1 blue
      , rect 0 ((model ^. #rows) - 1) (model ^. #columns) 1 blue
      , text (model ^. #myScorePos) 0 mempty mempty (show (model ^. #myScore))
      , text (model ^. #opScorePos) 0 mempty mempty (show (model ^. #opScore))
      -- , text 4 0 mempty mempty (show (model ^. #ball))
      ]  


--------------------------------------------------------------------------------
-- Tick
--------------------------------------------------------------------------------

tickEvery :: Model -> Maybe Seconds
-- tickEvery model = Just (1/gameSpeed)
tickEvery model =
  let 
    gamestate = model ^. #gameState
  in
    case gamestate of

      Hosting -> Just (1/gameSpeed)

      Joined -> Nothing

      NotPlaying -> Nothing


--------------------------------------------------------------------------------
-- Subscribe
--------------------------------------------------------------------------------

subscribe :: Model -> HashSet Text
subscribe _ =
  HashSet.singleton "Pong"


--------------------------------------------------------------------------------
-- Game
--------------------------------------------------------------------------------

game :: ElmGame Model ServerMsg
game =
  ElmGame init update view tickEvery subscribe
