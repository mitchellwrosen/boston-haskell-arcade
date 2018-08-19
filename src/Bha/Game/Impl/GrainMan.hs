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
  | LocPyramid
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
  | ExploreGrainDialog
  | ExploreWaterDialog
  | TradeDialog
  | TravelDialog

data Model
  = Model
  { modelLoc               :: Location
  , modelDialog            :: Dialog
  , modelWater             :: Int
  , modelGrain             :: Int
  , modelGold              :: Int
  , modelGrainWorldWater   :: Int -- Grain world's water
  , modelExploredWater     :: Int
  }

game :: ElmGame
game =
  ElmGame init update view tickEvery

init :: StdGen -> Model
init _seed =
  Model
    { modelLoc               = LocGrain
    , modelDialog            = ActionDialog
    , modelWater             = 0
    , modelGrain             = 0
    , modelGold              = 0
    , modelGrainWorldWater   = 0
    , modelExploredWater     = 0
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
              case modelLoc model of
                LocGrain ->
                  Just model
                    { modelDialog = ExploreGrainDialog
                    }
                LocWater ->
                  Just model
                    { modelDialog = ExploreWaterDialog
                    , modelExploredWater = modelExploredWater model + 1
                    }
                LocPyramid ->
                  error "explore pyramid"

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

        ExploreGrainDialog ->
          case List.lookup c (exploreGrainOptions model) of
            Nothing ->
              Just model

            Just () ->
              Just model
                { modelDialog = ActionDialog }

        ExploreWaterDialog ->
          case List.lookup c (exploreWaterOptions model) of
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
        { modelWater           = modelWater model - 1
        , modelGold            = modelGold model + n
        , modelGrainWorldWater = modelGrainWorldWater model + 1
        }

    Buy Water _n ->
      error "buy water?"
      -- model
      --   { modelWater = modelWater model + 1
      --   , modelGold  = modelGold model - n
      --   }

    Sell Grain _n ->
      error "sell grain?"
      -- model
      --   { modelGrain = modelGrain model - 1
      --   , modelGold  = modelGold model + n
      --   }

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
      (zip ['1'..] . catMaybes)
        [ Just Explore
        , do
            guard (length (tradeOptions model) > 1)
            pure Trade
        , Just Travel
        ]

    LocPyramid ->
      (zip ['1'..] . catMaybes)
        [ Just Trade
        , Just Travel
        ]

    LocWater ->
      (zip ['1'..] . catMaybes)
        [ Just Explore
        , do
            guard (isJust (tryScoopWater (modelWater model)))
            pure ScoopWater
        , Just Travel
        ]

grainWorldFixedWell :: Model -> Bool
grainWorldFixedWell model =
  modelGrainWorldWater model >= 15

exploreGrainOptions :: Model -> [(Char, ())]
exploreGrainOptions _model =
  zip ['1'..] [()]

exploreWaterOptions :: Model -> [(Char, ())]
exploreWaterOptions _model =
  zip ['1'..] [()]

tradeOptions :: Model -> [(Char, Maybe Trade)]
tradeOptions model =
  (zip ['1'..] . catMaybes)
    [ do
        guard (modelWater model > 0)
        guard (not (grainWorldFixedWell model))
        pure (Just (Sell Water 10))
    , do
        guard (modelGold model >= 5)
        pure (Just (Buy Grain 5))
    , pure Nothing
    ]

discoveredPyramid :: Model -> Bool
discoveredPyramid =
  (>= 5) . modelExploredWater

-- | Where are we allowed to travel, and what button do we press to get there?
travelOptions :: Model -> [(Char, Maybe Location)]
travelOptions model =
  case modelLoc model of
    LocGrain ->
      (zip ['1'..] . catMaybes)
        [ pure (Just LocWater)
        , do
            guard (discoveredPyramid model)
            pure (Just LocPyramid)
        , pure Nothing
        ]

    LocWater ->
      (zip ['1'..] . catMaybes)
        [ pure (Just LocGrain)
        , do
            guard (discoveredPyramid model)
            pure (Just LocPyramid)
        , pure Nothing
        ]

    LocPyramid ->
      zip ['1'..]
        [ Just LocGrain
        , Just LocWater
        , Nothing
        ]

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
  LocGrain   -> viewLocGrain
  LocPyramid -> viewLocPyramid
  LocWater   -> viewLocWater

viewLocGrain :: Cells
viewLocGrain =
  mconcat
    [ rect 0 0 100 20 mempty blue
    , rect 0 20 100 10 mempty yellow
    , rect 56 13 7 7 mempty white
    , rect 64 12 7 8 mempty white
    ]

viewLocPyramid :: Cells
viewLocPyramid =
  mconcat
    [ rect 0 0 100 20 mempty green

    , rect 44  6 12 2 mempty red
    , rect 40  8 20 2 mempty red
    , rect 36 10 28 2 mempty red
    , rect 32 12 36 2 mempty red
    , rect 28 14 44 2 mempty red
    , rect 24 16 52 2 mempty red
    , rect 20 18 60 2 mempty red

    , rect 0 20 100 10 mempty magenta
    ]

viewLocWater :: Cells
viewLocWater =
  mconcat
    [ rect 0 0 100 20 mempty blue
    , rect 0 20 100 10 mempty cyan
    ]

viewDialog :: Model -> Dialog -> Cells
viewDialog model = \case
  ActionDialog ->
    viewDialogOptions (map (second showAction) (actionOptions model))

  ExploreGrainDialog ->
    mconcat
      [ tbstr 5 3 black white
          "You meet a man. He says, \"Our well is broken. Do you have water?\""
      , viewDialogOptions (map (second showOk) (exploreGrainOptions model))
      ]

  ExploreWaterDialog ->
    mconcat
      [ if modelExploredWater model == 5
          then
            mconcat
             [ tbstr 5 2 black white "You find the wreckage of an ancient orbital booster."
             , tbstr 5 3 black white "By copying the design, you improve your engines."
             ]
            else
              tbstr 5 3 black white "Inky blackness! Luminous fish lights pass by."
      , viewDialogOptions (map (second showOk) (exploreWaterOptions model))
      ]

  TradeDialog ->
    (mconcat . catMaybes)
      [ do
          guard (grainWorldFixedWell model)
          pure (tbstr 5 3 black white "We fixed our well and the drought is over. We aren't buying any water.")
      , Just (viewDialogOptions (map (second showTrade) (tradeOptions model)))
      ]

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
    Nothing         -> "Cancel"
    Just LocGrain   -> "Grain world"
    Just LocPyramid -> "Pyramid world"
    Just LocWater   -> "Water world"

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
