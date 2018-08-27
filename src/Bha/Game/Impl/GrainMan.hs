{-# LANGUAGE FlexibleInstances, FunctionalDependencies, LambdaCase,
             MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings,
             TemplateHaskell, TupleSections #-}

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
  { _modelLocL               :: Location
  , _modelDialogL            :: Dialog
  , _modelWaterL             :: Int
  , _modelGrainL             :: Int
  , _modelGoldL              :: Int
  , _modelGrainWorldWaterL   :: Int -- Grain world's water
  , _modelExploredWaterL     :: Int
  }
makeFields ''Model

data ModelView
  = ModelView
  { _modelViewLocL    :: Location
  , _modelViewDialogL :: ([String], [String])
  , _modelViewWaterL  :: Int
  , _modelViewGrainL  :: Int
  , _modelViewGoldL   :: Int
  }
makeFields ''ModelView

modelToModelView :: Model -> ModelView
modelToModelView model =
  ModelView
    { _modelViewDialogL =
        case model ^. dialogL of
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

    , _modelViewLocL   = model ^. locL
    , _modelViewWaterL = model ^. waterL
    , _modelViewGrainL = model ^. grainL
    , _modelViewGoldL  = model ^. goldL
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
    { _modelLocL               = LocGrain
    , _modelDialogL            = ActionDialog
    , _modelWaterL             = 0
    , _modelGrainL             = 0
    , _modelGoldL              = 0
    , _modelGrainWorldWaterL   = 0
    , _modelExploredWaterL     = 0
    }

update :: Either NominalDiffTime Event -> StateT Model Maybe ()
update = \case
  Left _ ->
    pure ()

  Right (EventKey KeyEsc _) ->
    empty

  Right (EventKey (KeyChar c) _) -> do
    model <- get
    case model ^. dialogL of
      ActionDialog ->
        put (handleAction c model)

      ExploreDialog ->
        put (handleExplore c model)

      TradeDialog ->
        put (handleTrade c model)

      TravelDialog ->
        put (handleTravel c model)

  Right _ ->
    pure ()

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
applyTrade trade =
  case trade of
    Sell Water n ->
      (waterL           %~ subtract 1) .
      (goldL            %~ (+ n)) .
      (grainWorldWaterL %~ (+1))

    Buy Water _n ->
      error "buy water?"

    Sell Grain _n ->
      error "sell grain?"

    Buy Grain n ->
      (grainL %~ (+1)) .
      (goldL  %~ (subtract n))

tryScoopWater :: Int -> Maybe Int
tryScoopWater n = do
  guard (n < 5)
  pure (n+1)

actionDialog :: Model -> ([String], [(String, Model)])
actionDialog model =
  case model ^. locL of
    LocGrain ->
      (noInfo . catMaybes)
        [ Just ("Explore", model & dialogL .~ ExploreDialog)
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
            & dialogL .~ ExploreDialog
            & exploredWaterL %~ (+1))

        , do
            guard (isJust (tryScoopWater (model ^. waterL)))
            Just ("Scoop water",
              model
                & waterL .~
                    fromMaybe
                      (model ^. waterL)
                      (tryScoopWater (model ^. waterL)))

        , Just doTravel
        ]

 where
  noInfo :: [(String, Model)] -> ([String], [(String, Model)])
  noInfo = ([],)

  doTrade :: (String, Model)
  doTrade =
    ("Trade", model & dialogL .~ TradeDialog)

  doTravel :: (String, Model)
  doTravel =
    ("Travel", model & dialogL .~ TravelDialog)

exploreDialog :: Model -> ([String], [(String, Model)])
exploreDialog model =
  (info, [("Ok", model & dialogL .~ ActionDialog)])
 where
  info :: [String]
  info =
    case model ^. locL of
      LocGrain ->
        [ "You meet a man. He says, \"Our well is broken. Do you have water?\"" ]

      LocWater ->
        if model ^. exploredWaterL == 5
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
          guard (model ^. waterL > 0)
          guard (not (grainWorldFixedWell model))
          let trade = Just (Sell Water 10)
          Just (showTrade trade, maybe id applyTrade trade model)
      , do
          guard (model ^. goldL >= 5)
          let trade = Just (Buy Grain 5)
          Just (showTrade trade, maybe id applyTrade trade model)
      , Just (showTrade Nothing, model & dialogL .~  ActionDialog)
      ]

travelDialog :: Model -> ([String], [(String, Model)])
travelDialog model =
  (info, options)
 where
  info :: [String]
  info = []

  options :: [(String, Model)]
  options =
    case model ^. locL of
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
      ( "Grain world"
      , model
          & locL .~ LocGrain
          & dialogL .~ ActionDialog
      )

    doWater :: (String, Model)
    doWater =
      ( "Water world"
      , model
          & locL .~ LocWater
          & dialogL .~ ActionDialog
      )

    doPyramid :: (String, Model)
    doPyramid =
      ( "Pyramid world"
      , model
          & locL .~ LocPyramid
          & dialogL .~ ActionDialog
      )

    doCancel :: (String, Model)
    doCancel =
      ( "Cancel"
      , model
          & dialogL .~ ActionDialog
      )

grainWorldFixedWell :: Model -> Bool
grainWorldFixedWell model =
  model ^. grainWorldWaterL >= 15

discoveredPyramid :: Model -> Bool
discoveredPyramid =
  (>= 5) . (^. exploredWaterL)

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
      [ viewLoc (model ^. locL)
      -- , viewInfo (modelViewInfo model)
      , viewDialog (model ^. dialogL)
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
        guard (model ^. goldL > 0)
        pure ("gold " ++ show (model ^. goldL))

    , do
        guard (model ^. grainL > 0)
        pure ("grain " ++ show (model ^. grainL))

    , do
        guard (model ^. waterL > 0)
        pure ("water " ++ show (model ^. waterL))
    ]

tickEvery :: Model -> Maybe NominalDiffTime
tickEvery _ =
  Nothing
