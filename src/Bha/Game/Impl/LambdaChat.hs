{-# LANGUAGE TemplateHaskell #-}

module Bha.Game.Impl.LambdaChat
  ( game
  ) where

import Data.Sequence (Seq)

import qualified Data.HashSet  as HashSet
import qualified Data.Sequence as Seq
import qualified Data.Text     as Text

import Bha.Elm.Prelude


--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

data Model
  = Model
  { _modelInputL :: !Text
  , _modelChatL  :: !(Seq Text)
  } deriving (Show)
makeFields ''Model

init :: Init Text Model
init =
  pure Model
    { _modelInputL = ""
    , _modelChatL  = mempty
    }


--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------

update :: Input Text -> Update Model Text ()
update = \case
  Key KeyEsc ->
    gameover

  Key (KeyChar c) ->
    inputL %= (`snoc` c)

  Key KeySpace ->
    inputL %= (`snoc` ' ')

  Key KeyBackspace2 ->
    inputL %= \s ->
      if Text.null s
        then s
        else Text.init s

  Key KeyEnter -> do
    input <- use inputL
    inputL .= mempty
    updateChatLog input
    send "chat" input

  Message _topic msg ->
    updateChatLog msg

  _ ->
    pure ()

updateChatLog :: Text -> Update Model Text ()
updateChatLog msg =
  chatL %= \chat ->
    if length chat == 15
      then snoc (Seq.drop 1 chat) msg
      else snoc chat msg


--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

view :: Model -> Scene
view model =
  Scene (render model) NoCursor

render :: Model -> Cells
render model =
  mconcat
    [ renderChat (model ^. chatL)
    , renderInput (model ^. inputL)
    ]

renderChat :: Seq Text -> Cells
renderChat =
  foldMap (\(i, s) -> text 0 i mempty mempty (Text.unpack s)) . zip [0..] .
    toList

renderInput :: Text -> Cells
renderInput s =
  text 0 15 mempty mempty ("> " ++ Text.unpack s)


--------------------------------------------------------------------------------
-- Tick
--------------------------------------------------------------------------------

tickEvery :: Model -> Maybe Seconds
tickEvery _ =
  Nothing


--------------------------------------------------------------------------------
-- Subscribe
--------------------------------------------------------------------------------

subscribe :: Model -> HashSet Text
subscribe _ =
  HashSet.singleton "chat"


--------------------------------------------------------------------------------
-- Game
--------------------------------------------------------------------------------

game :: ElmGame Model Text
game =
  ElmGame init update view tickEvery subscribe
