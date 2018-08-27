{-# LANGUAGE TupleSections, LambdaCase, NoImplicitPrelude, OverloadedStrings #-}

module Bha.Game.Impl.GrainMan
  ( game
  ) where

import Bha.Elm.Prelude
import Bha.View

import qualified Data.List as List

data Location
  = LocGrain
  | LocPyramid
  | LocWater

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
  { modelLoc               :: Location
  , modelDialog            :: Dialog
  , modelWater             :: Int
  , modelGrain             :: Int
  , modelGold              :: Int
  , modelGrainWorldWater   :: Int -- Grain world's water
  , modelExploredWater     :: Int
  }

data ModelView
  = ModelView
  { modelViewLoc    :: Location
  , modelViewDialog :: ([String], [String])
  , modelViewWater  :: Int
  , modelViewGrain  :: Int
  , modelViewGold   :: Int
  }

modelToModelView :: Model -> ModelView
modelToModelView model =
  ModelView
    { modelViewDialog =
        case modelDialog model of
          ActionDialog ->
            case actionDialog model of
              (x, y) ->
                (x, map fst y)

          ExploreDialog ->
            case exploreDialog model of
              (x, y) ->
                (x, map fst y)

          TradeDialog ->
            case tradeDialog model of
              (x, y) ->
                (x, map fst y)

          TravelDialog ->
            case travelDialog model of
              (x, y) ->
                (x, map fst y)

    , modelViewLoc   = modelLoc   model
    , modelViewWater = modelWater model
    , modelViewGrain = modelGrain model
    , modelViewGold  = modelGold  model
    }

showResource :: Resource -> String
showResource = \case
  Grain -> "grain"
  Water -> "water"

showTrade :: Maybe Trade -> String
showTrade = \case
  Nothing           -> "Goodbye"
  Just (Sell res n) -> "Sell " ++ showResource res ++ " for " ++ show n
  Just (Buy res n)  -> "Buy "  ++ showResource res ++ " for " ++ show n

game :: ElmGame Model
game =
  ElmGame init update view tickEvery

init :: Seed -> Model
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
          Just (handleAction c model)

        ExploreDialog ->
          Just (handleExplore c model)

        TradeDialog ->
          Just (handleTrade c model)

        TravelDialog ->
          Just (handleTravel c model)

    Right _ ->
      Just model

handleAction :: Char -> Model -> Model
handleAction c model =
  maybe
    model
    snd
    (List.lookup c (zip ['1'..] (snd (actionDialog model))))

handleExplore :: Char -> Model -> Model
handleExplore c model =
  maybe
    model
    snd
    (List.lookup c (zip ['1'..] (snd (exploreDialog model))))

handleTrade :: Char -> Model -> Model
handleTrade c model =
  maybe
    model
    snd
    (List.lookup c (zip ['1'..] (snd (tradeDialog model))))

handleTravel :: Char -> Model -> Model
handleTravel c model =
  maybe
    model
    snd
    (List.lookup c (zip ['1'..] (snd (travelDialog model))))

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

actionDialog :: Model -> ([String], [(String, Model)])
actionDialog model =
  case modelLoc model of
    LocGrain ->
      (noInfo . catMaybes)
        [ Just ("Explore", model { modelDialog = ExploreDialog })
        , do
            guard (length (snd (tradeDialog model)) > 1)
            Just doTrade
        , Just doTravel
        ]

    LocPyramid ->
      noInfo [ doTrade, doTravel ]

    LocWater ->
      (noInfo . catMaybes)
        [ Just ("Explore", model
            { modelDialog = ExploreDialog
            , modelExploredWater = modelExploredWater model + 1
            })

        , do
            guard (isJust (tryScoopWater (modelWater model)))
            Just ("Scoop water",
              model
                { modelWater =
                    fromMaybe
                      (modelWater model)
                      (tryScoopWater (modelWater model))
                })

        , Just doTravel
        ]

 where
  noInfo :: [(String, Model)] -> ([String], [(String, Model)])
  noInfo = ([],)

  doTrade :: (String, Model)
  doTrade =
    ("Trade", model { modelDialog = TradeDialog })

  doTravel :: (String, Model)
  doTravel =
    ("Travel", model { modelDialog = TravelDialog })

exploreDialog :: Model -> ([String], [(String, Model)])
exploreDialog model =
  (info, [("Ok", model { modelDialog = ActionDialog })])
 where
  info :: [String]
  info =
    case modelLoc model of
      LocGrain ->
        [ "You meet a man. He says, \"Our well is broken. Do you have water?\"" ]

      LocWater ->
        if modelExploredWater model == 5
          then
            [ "You find the wreckage of an ancient orbital booster."
            , "By copying the design, you improve your engines."
            ]
          else
            [ "Inky blackness! Luminous fish lights pass by." ]

      LocPyramid ->
        error "explore in LocPyramid"

tradeDialog :: Model -> ([String], [(String, Model)])
tradeDialog model =
  (info, options)
 where
  info :: [String]
  info = do
    guard (grainWorldFixedWell model)
    [ "We fixed our well and the drought is over. We aren't buying any water." ]

  options :: [(String, Model)]
  options =
    catMaybes
      [ do
          guard (modelWater model > 0)
          guard (not (grainWorldFixedWell model))
          let trade = Just (Sell Water 10)
          Just (showTrade trade, maybe id applyTrade trade model)
      , do
          guard (modelGold model >= 5)
          let trade = Just (Buy Grain 5)
          Just (showTrade trade, maybe id applyTrade trade model)
      , Just (showTrade Nothing, model { modelDialog = ActionDialog })
      ]

travelDialog :: Model -> ([String], [(String, Model)])
travelDialog model =
  (info, options)
 where
  info :: [String]
  info = []

  options :: [(String, Model)]
  options =
    case modelLoc model of
      LocGrain ->
        catMaybes
          [ pure doWater
          , do
              guard (discoveredPyramid model)
              pure doPyramid
          , pure doCancel
          ]

      LocWater ->
        catMaybes
          [ pure doGrain
          , do
              guard (discoveredPyramid model)
              pure doPyramid
          , pure doCancel
          ]

      LocPyramid ->
        [ doGrain
        , doWater
        , doCancel
        ]

   where
    doGrain :: (String, Model)
    doGrain =
      ("Grain world",
        model
          { modelLoc = LocGrain
          , modelDialog = ActionDialog
          })

    doWater :: (String, Model)
    doWater =
      ("Water world",
        model
          { modelLoc = LocWater
          , modelDialog = ActionDialog
          })

    doPyramid :: (String, Model)
    doPyramid =
      ("Pyramid world",
        model
          { modelLoc = LocPyramid
          , modelDialog = ActionDialog
          })

    doCancel :: (String, Model)
    doCancel =
      ("Cancel",
        model
          { modelDialog = ActionDialog
          })

grainWorldFixedWell :: Model -> Bool
grainWorldFixedWell model =
  modelGrainWorldWater model >= 15

discoveredPyramid :: Model -> Bool
discoveredPyramid =
  (>= 5) . modelExploredWater

view :: Model -> Scene
view =
  viewModel . modelToModelView

viewModel :: ModelView -> Scene
viewModel model =
  Scene cells NoCursor
 where
  cells :: Cells
  cells =
    mconcat
      [ viewLoc (modelViewLoc model)
      -- , viewInfo (modelViewInfo model)
      , viewDialog (modelViewDialog model)
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

viewDialog :: ([String], [String]) -> Cells
viewDialog (info, options) =
  viewInfo info <> viewOptions options

viewInfo :: [String] -> Cells
viewInfo = \case
  [] ->
    mempty

  [s] ->
    tbstr 5 3 black white s

  [s1, s2] ->
    tbstr 5 2 black white s1 <>
    tbstr 5 3 black white s2

  _ ->
    error "viewInfo"

viewOptions :: [String] -> Cells
viewOptions =
  foldMap f . zip [0..]
 where
  f :: (Int, String) -> Cells
  f (i, s) =
    tbstr 5 (i+5) black white ('(' : show (i+1) ++ ") " ++ s)

viewHud :: ModelView -> Cells
viewHud model =
  case catMaybes blah of
    [] -> mempty
    ss -> tbstr 0 30 mempty mempty (unwords ss)

 where
  blah :: [Maybe String]
  blah =
    [ do
        guard (modelViewGold model > 0)
        pure ("gold " ++ show (modelViewGold model))

    , do
        guard (modelViewGrain model > 0)
        pure ("grain " ++ show (modelViewGrain model))

    , do
        guard (modelViewWater model > 0)
        pure ("water " ++ show (modelViewWater model))
    ]

tickEvery :: Model -> Maybe NominalDiffTime
tickEvery _ =
  Nothing
