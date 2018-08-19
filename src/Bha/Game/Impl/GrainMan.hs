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

data Action
  = Explore
  | ScoopWater
  | Trade
  | Travel

data Trade
  = Buy Resource Int
  | Sell Resource Int

data Resource
  = Grain
  | Water

data Dialog
  = ActionDialog
  | ExploreDialog
  | TradeDialog
  | TravelDialog

data Model
  = Model
  { modelLoc    :: Location
  , modelDialog :: Dialog
  , modelWater  :: Int
  , modelGrain  :: Int
  , modelGold   :: Int
  }

game :: ElmGame
game =
  ElmGame init update view tickEvery

init :: StdGen -> Model
init _seed =
  Model
    { modelLoc    = LocGrain
    , modelDialog = ActionDialog
    , modelWater  = 0
    , modelGrain  = 0
    , modelGold   = 0
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
        ActionDialog ->
          case List.lookup c (actionOptions model) of
            Nothing ->
              Just model

            Just Explore ->
              Just model
                { modelDialog = ExploreDialog
                }

            Just ScoopWater ->
              Just model
                { modelWater =
                    fromMaybe
                      (modelWater model)
                      (tryScoopWater (modelWater model))
                }

            Just Trade ->
              Just model
                { modelDialog = TradeDialog
                }

            Just Travel ->
              Just model
                { modelDialog = TravelDialog
                }

        ExploreDialog ->
          case List.lookup c (exploreOptions model) of
            Nothing ->
              Just model

            Just () ->
              Just model
                { modelDialog = ActionDialog }

        TradeDialog ->
          case List.lookup c (tradeOptions model) of
            Nothing ->
              Just model

            Just Nothing ->
              Just model
                { modelDialog = ActionDialog
                }

            Just (Just trade) ->
              Just (applyTrade trade model)

        TravelDialog ->
          case List.lookup c (travelOptions model) of
            Nothing ->
              Just model

            Just loc ->
              case loc of
                Nothing ->
                  Just model
                    { modelDialog = ActionDialog
                    }

                Just loc' ->
                  Just model
                    { modelDialog = ActionDialog
                    , modelLoc = loc'
                    }

    Right _ ->
      Just model

applyTrade :: Trade -> Model -> Model
applyTrade trade model =
  case trade of
    Sell Water n ->
      model
        { modelWater = modelWater model - 1
        , modelGold  = modelGold model + n
        }

    Buy Water n ->
      model
        { modelWater = modelWater model + 1
        , modelGold  = modelGold model - n
        }

    Sell Grain n ->
      model
        { modelGrain = modelGrain model - 1
        , modelGold  = modelGold model + n
        }

    Buy Grain n ->
      model
        { modelGrain = modelGrain model + 1
        , modelGold  = modelGold model - n
        }

tryScoopWater :: Int -> Maybe Int
tryScoopWater n = do
  guard (n < 5)
  pure (n+1)

actionOptions :: Model -> [(Char, Action)]
actionOptions model =
  case modelLoc model of
    LocGrain ->
      (zip ['a'..] . catMaybes)
        [ Just Explore
        , do
            guard (length (tradeOptions model) > 1)
            pure Trade
        , Just Travel
        ]

    LocWater ->
      (zip ['a'..] . catMaybes)
        [ Just Travel
        , do
            guard (isJust (tryScoopWater (modelWater model)))
            pure ScoopWater
        ]

exploreOptions :: Model -> [(Char, ())]
exploreOptions _model =
  zip ['a'..] [()]

tradeOptions :: Model -> [(Char, Maybe Trade)]
tradeOptions model =
  (zip ['a'..] . catMaybes)
    [ do
        guard (modelWater model > 0)
        pure (Just (Sell Water 10))
    , do
        guard (modelGold model >= 5)
        pure (Just (Buy Grain 5))
    , pure Nothing
    ]

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
    mconcat
      [ viewLoc (modelLoc model)
      , viewDialog model (modelDialog model)
      , viewHud model
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
  ActionDialog ->
    viewDialogOptions (map (second showAction) (actionOptions model))

  ExploreDialog ->
    mconcat
      [ tbstr 5 3 black white
          "You meet a man. He says, \"Our well is broken. Do you have water?\""
      , viewDialogOptions (map (second showOk) (exploreOptions model))
      ]

  TradeDialog ->
    viewDialogOptions (map (second showTrade) (tradeOptions model))

  TravelDialog ->
    viewDialogOptions (map (second showLoc) (travelOptions model))
 where
  showAction :: Action -> String
  showAction = \case
    Explore    -> "Explore"
    ScoopWater -> "Scoop water"
    Trade      -> "Trade"
    Travel     -> "Travel"

  showOk :: () -> String
  showOk () =
    "Ok"

  showLoc :: Maybe Location -> String
  showLoc = \case
    Nothing       -> "Cancel"
    Just LocGrain -> "Grain World"
    Just LocWater -> "Water World"

  showTrade :: Maybe Trade -> String
  showTrade = \case
    Nothing           -> "Goodbye"
    Just (Sell res n) -> "Sell " ++ showResource res ++ " for " ++ show n
    Just (Buy res n)  -> "Buy "  ++ showResource res ++ " for " ++ show n

  showResource :: Resource -> String
  showResource = \case
    Grain -> "grain"
    Water -> "water"

viewDialogOptions :: [(Char, String)] -> Cells
viewDialogOptions =
  foldMap (\(i, (c,s)) -> tbstr 5 (i+5) black white ('(':c:')':' ':s))
    . zip [0..]

viewHud :: Model -> Cells
viewHud model =
  case catMaybes blah of
    [] -> mempty
    ss -> tbstr 0 30 mempty mempty (unwords ss)

 where
  blah :: [Maybe String]
  blah =
    [ do
        guard (modelGold model > 0)
        pure ("gold " ++ show (modelGold model))

    , do
        guard (modelGrain model > 0)
        pure ("grain " ++ show (modelGrain model))

    , do
        guard (modelWater model > 0)
        pure ("water " ++ show (modelWater model))
    ]

tickEvery :: Model -> Maybe NominalDiffTime
tickEvery _ =
  Nothing
