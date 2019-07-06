{-# LANGUAGE TemplateHaskell #-}

module Bha.Game.Impl.LambdaChat
  ( game
  ) where

import Bha.Elm.Prelude

import qualified Data.HashSet  as HashSet
import qualified Data.List     as List
import qualified Data.Sequence as Seq
import qualified Data.Text     as Text


--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

data Model
  = Model
  { input :: Text
  , chat  :: (Seq Text)
  } deriving stock (Generic, Show)

init :: Int -> Int -> Init Text Model
init _ _ =
  pure Model
    { input = ""
    , chat  = mempty
    }


--------------------------------------------------------------------------------
-- Update
--------------------------------------------------------------------------------

update :: Input Text -> Update Model Text ()
update = \case
  Key KeyEsc ->
    gameover

  Key (KeyChar c) ->
    #input %= (`snoc` c)

  Key KeySpace ->
    #input %= (`snoc` ' ')

  Key KeyBackspace2 ->
    #input %= \s ->
      if Text.null s
        then s
        else Text.init s

  Key KeyEnter -> do
    input <- use #input
    #input .= mempty
    updateChatLog input
    send "chat" input

  Message _topic msg ->
    updateChatLog msg

  _ ->
    pure ()

updateChatLog :: Text -> Update Model Text ()
updateChatLog msg =
  #chat %= \chat ->
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
    [ renderChat (model ^. #chat)
    , renderInput (model ^. #input)
    ]

renderChat :: Seq Text -> Cells
renderChat =
  toList
    >>> List.zip [0..]
    >>> foldMap (\(i, s) -> text 0 i mempty mempty (Text.unpack s))

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
