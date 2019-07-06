module Bha.Game.Impl.GrainMan
  ( game
  ) where

import Bha.Elm.Prelude

import qualified Data.List as List


data Location
  = LocGrain
  | LocPyramid
  | LocWater
  deriving (Show)

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
  deriving (Show)

data Model
  = Model
  { loc             :: Location
  , dialog          :: Dialog
  , water           :: Int
  , grain           :: Int
  , gold            :: Int
  , grainWorldWater :: Int -- Grain world's water
  , exploredWater   :: Int
  } deriving stock (Generic, Show)

data ModelView
  = ModelView
  { loc    :: Location
  , dialog :: ([[Char]], [[Char]])
  , water  :: Int
  , grain  :: Int
  , gold   :: Int
  } deriving stock (Generic)

modelToModelView :: Model -> ModelView
modelToModelView model =
  ModelView
    { dialog =
        case model ^. #dialog of
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

    , loc   = model ^. #loc
    , water = model ^. #water
    , grain = model ^. #grain
    , gold  = model ^. #gold
    }

showResource :: Resource -> [Char]
showResource = \case
  Grain -> "grain"
  Water -> "water"

showTrade :: Maybe Trade -> [Char]
showTrade = \case
  Nothing           -> "Goodbye"
  Just (Sell res n) -> "Sell " ++ showResource res ++ " for " ++ show n
  Just (Buy res n)  -> "Buy "  ++ showResource res ++ " for " ++ show n

game :: ElmGame Model Void
game =
  ElmGame init update view tickEvery subscribe

init :: Init Void Model
init =
  pure Model
    { loc               = LocGrain
    , dialog            = ActionDialog
    , water             = 0
    , grain             = 0
    , gold              = 0
    , grainWorldWater   = 0
    , exploredWater     = 0
    }

update :: Input Void -> Update Model Void ()
update = \case
  Key KeyEsc ->
    gameover

  Key (KeyChar c) -> do
    model <- get
    case model ^. #dialog of
      ActionDialog ->
        put (handleAction c model)

      ExploreDialog ->
        put (handleExplore c model)

      TradeDialog ->
        put (handleTrade c model)

      TravelDialog ->
        put (handleTravel c model)

  _ ->
    pure ()

handleAction :: Char -> Model -> Model
handleAction c model =
  maybe
    model
    snd
    (List.lookup c (List.zip ['1'..] (snd (actionDialog model))))

handleExplore :: Char -> Model -> Model
handleExplore c model =
  maybe
    model
    snd
    (List.lookup c (List.zip ['1'..] (snd (exploreDialog model))))

handleTrade :: Char -> Model -> Model
handleTrade c model =
  maybe
    model
    snd
    (List.lookup c (List.zip ['1'..] (snd (tradeDialog model))))

handleTravel :: Char -> Model -> Model
handleTravel c model =
  maybe
    model
    snd
    (List.lookup c (List.zip ['1'..] (snd (travelDialog model))))

applyTrade :: Trade -> Model -> Model
applyTrade trade =
  case trade of
    Sell Water n ->
      (#water           %~ subtract 1) .
      (#gold            %~ (+ n)) .
      (#grainWorldWater %~ (+1))

    Buy Water _n ->
      error "buy water?"

    Sell Grain _n ->
      error "sell grain?"

    Buy Grain n ->
      (#grain %~ (+1)) .
      (#gold  %~ (subtract n))

tryScoopWater :: Int -> Maybe Int
tryScoopWater n = do
  guard (n < 5)
  pure (n+1)

actionDialog :: Model -> ([[Char]], [([Char], Model)])
actionDialog model =
  case model ^. #loc of
    LocGrain ->
      (noInfo . catMaybes)
        [ Just ("Explore", model & #dialog .~ ExploreDialog)
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
            & #dialog .~ ExploreDialog
            & #exploredWater %~ (+1))

        , do
            guard (is _Just (tryScoopWater (model ^. #water)))
            Just ("Scoop water",
              model
                & #water .~
                    fromMaybe
                      (model ^. #water)
                      (tryScoopWater (model ^. #water)))

        , Just doTravel
        ]

 where
  noInfo :: [([Char], Model)] -> ([[Char]], [([Char], Model)])
  noInfo = ([],)

  doTrade :: ([Char], Model)
  doTrade =
    ("Trade", model & #dialog .~ TradeDialog)

  doTravel :: ([Char], Model)
  doTravel =
    ("Travel", model & #dialog .~ TravelDialog)

exploreDialog :: Model -> ([[Char]], [([Char], Model)])
exploreDialog model =
  (info, [("Ok", model & #dialog .~ ActionDialog)])
 where
  info :: [[Char]]
  info =
    case model ^. #loc of
      LocGrain ->
        [ "You meet a man. He says, \"Our well is broken. Do you have water?\"" ]

      LocWater ->
        if model ^. #exploredWater == 5
          then
            [ "You find the wreckage of an ancient orbital booster."
            , "By copying the design, you improve your engines."
            ]
          else
            [ "Inky blackness! Luminous fish lights pass by." ]

      LocPyramid ->
        error "explore in LocPyramid"

tradeDialog :: Model -> ([[Char]], [([Char], Model)])
tradeDialog model =
  (info, options)
 where
  info :: [[Char]]
  info = do
    guard (grainWorldFixedWell model)
    [ "We fixed our well and the drought is over. We aren't buying any water." ]

  options :: [([Char], Model)]
  options =
    catMaybes
      [ do
          guard (model ^. #water > 0)
          guard (not (grainWorldFixedWell model))
          let trade = Just (Sell Water 10)
          Just (showTrade trade, maybe id applyTrade trade model)
      , do
          guard (model ^. #gold >= 5)
          let trade = Just (Buy Grain 5)
          Just (showTrade trade, maybe id applyTrade trade model)
      , Just (showTrade Nothing, model & #dialog .~  ActionDialog)
      ]

travelDialog :: Model -> ([[Char]], [([Char], Model)])
travelDialog model =
  (info, options)
 where
  info :: [[Char]]
  info = []

  options :: [([Char], Model)]
  options =
    case model ^. #loc of
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
    doGrain :: ([Char], Model)
    doGrain =
      ( "Grain world"
      , model
          & #loc .~ LocGrain
          & #dialog .~ ActionDialog
      )

    doWater :: ([Char], Model)
    doWater =
      ( "Water world"
      , model
          & #loc .~ LocWater
          & #dialog .~ ActionDialog
      )

    doPyramid :: ([Char], Model)
    doPyramid =
      ( "Pyramid world"
      , model
          & #loc .~ LocPyramid
          & #dialog .~ ActionDialog
      )

    doCancel :: ([Char], Model)
    doCancel =
      ( "Cancel"
      , model
          & #dialog .~ ActionDialog
      )

grainWorldFixedWell :: Model -> Bool
grainWorldFixedWell model =
  model ^. #grainWorldWater >= 15

discoveredPyramid :: Model -> Bool
discoveredPyramid =
  (>= 5) . (^. #exploredWater)

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
      [ viewLoc (model ^. #loc)
      -- , viewInfo (modelViewInfo model)
      , viewDialog (model ^. #dialog)
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
    [ rect 0 0 100 20 blue
    , rect 0 20 100 10 yellow
    , rect 56 13 7 7 white
    , rect 64 12 7 8 white
    ]

viewLocPyramid :: Cells
viewLocPyramid =
  mconcat
    [ rect 0 0 100 20 green

    , rect 44  6 12 2 red
    , rect 40  8 20 2 red
    , rect 36 10 28 2 red
    , rect 32 12 36 2 red
    , rect 28 14 44 2 red
    , rect 24 16 52 2 red
    , rect 20 18 60 2 red

    , rect 0 20 100 10 magenta
    ]

viewLocWater :: Cells
viewLocWater =
  mconcat
    [ rect 0 0 100 20 blue
    , rect 0 20 100 10 cyan
    ]

viewDialog :: ([[Char]], [[Char]]) -> Cells
viewDialog (info, options) =
  viewInfo info <> viewOptions options

viewInfo :: [[Char]] -> Cells
viewInfo = \case
  [] ->
    mempty

  [s] ->
    text 5 3 black white s

  [s1, s2] ->
    text 5 2 black white s1 <>
    text 5 3 black white s2

  _ ->
    error "viewInfo"

viewOptions :: [[Char]] -> Cells
viewOptions =
  foldMap f . List.zip [0..]
 where
  f :: (Int, [Char]) -> Cells
  f (i, s) =
    text 5 (i+5) black white ('(' : show (i+1) ++ ") " ++ s)

viewHud :: ModelView -> Cells
viewHud model =
  case catMaybes blah of
    [] -> mempty
    ss -> text 0 30 mempty mempty (List.unwords ss)

 where
  blah :: [Maybe [Char]]
  blah =
    [ do
        guard (model ^. #gold > 0)
        pure ("gold " ++ show (model ^. #gold))

    , do
        guard (model ^. #grain > 0)
        pure ("grain " ++ show (model ^. #grain))

    , do
        guard (model ^. #water > 0)
        pure ("water " ++ show (model ^. #water))
    ]

tickEvery :: Model -> Maybe Seconds
tickEvery _ =
  Nothing

subscribe :: Model -> HashSet Text
subscribe _ =
  mempty
