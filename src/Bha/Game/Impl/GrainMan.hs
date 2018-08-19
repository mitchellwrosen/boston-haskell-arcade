{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings #-}

module Bha.Game.Impl.GrainMan
  ( game
  ) where

import Bha.Elm.Prelude
import Bha.View

import Data.Bifunctor (second)

import qualified Data.List as List

data Location
  = LocGrain
  | LocWater

data Dialog
  = TravelDialog

data Model
  = Model
  { modelLoc :: Location
  , modelDialog :: Maybe Dialog
  }

game :: ElmGame
game =
  ElmGame init update view tickEvery

init :: StdGen -> Model
init seed =
  Model
    { modelLoc = LocGrain
    , modelDialog = Nothing
    }

update :: Either NominalDiffTime Event -> Model -> Maybe Model
update event model =
  case event of
    Left _ ->
      Just model

    Right (EventKey KeyEsc _) ->
      Nothing

    Right (EventKey (KeyChar c) _) ->
      case modelDialog model of
        Nothing ->
          case c of
            't' ->
              Just model { modelDialog = Just TravelDialog }

            _ ->
              Just model

        Just TravelDialog ->
          case List.lookup c (travelOptions model) of
            Nothing ->
              Just model

            Just loc ->
              case loc of
                Nothing ->
                  Just model
                    { modelDialog = Nothing
                    }

                Just loc' ->
                  Just model
                    { modelDialog = Nothing
                    , modelLoc = loc'
                    }

-- | Where are we allowed to travel, and what button do we press to get there?
travelOptions :: Model -> [(Char, Maybe Location)]
travelOptions model =
  case modelLoc model of
    LocGrain -> zip ['a'..] [Just LocWater, Nothing]
    LocWater -> zip ['a'..] [Just LocGrain, Nothing]

view :: Model -> Scene
view model =
  Scene cells NoCursor
 where
  cells :: Cells
  cells =
    (mconcat . catMaybes)
      [ Just (viewLoc (modelLoc model))
      , viewDialog model <$> modelDialog model
      ]

viewLoc :: Location -> Cells
viewLoc = \case
  LocGrain -> viewLocGrain
  LocWater -> viewLocWater

viewLocGrain :: Cells
viewLocGrain =
  mconcat
    [ foldMap
        (\(r, c) -> set c r (Cell ' ' mempty blue))
        ((,) <$> [0..19] <*> [0..99])

    , foldMap
        (\(r, c) -> set c r (Cell ' ' mempty yellow))
        ((,) <$> [20..29] <*> [0..99])

    , foldMap
        (\(r, c) -> set c r (Cell ' ' mempty white))
        ((,) <$> [13..19] <*> [56..62])

    , foldMap
        (\(r, c) -> set c r (Cell ' ' mempty white))
        ((,) <$> [12..19] <*> [64..70])
    ]

viewLocWater :: Cells
viewLocWater =
  mconcat
    [ foldMap
        (\(r, c) -> set c r (Cell ' ' mempty blue))
        ((,) <$> [0..19] <*> [0..99])

    , foldMap
        (\(r, c) -> set c r (Cell ' ' mempty cyan))
        ((,) <$> [20..29] <*> [0..99])
    ]

viewDialog :: Model -> Dialog -> Cells
viewDialog model = \case
  TravelDialog ->
    viewDialogOptions (map (second showLoc) (travelOptions model))
 where
  showLoc :: Maybe Location -> String
  showLoc = \case
    Nothing       -> "Cancel"
    Just LocGrain -> "Grain World"
    Just LocWater -> "Water World"

viewDialogOptions :: [(Char, String)] -> Cells
viewDialogOptions =
  foldMap (\(i, (c,s)) -> tbstr 5 (i+5) black white ('(':c:')':' ':s))
    . zip [0..]

tickEvery :: Model -> Maybe NominalDiffTime
tickEvery _ =
  Nothing
