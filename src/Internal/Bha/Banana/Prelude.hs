module Internal.Bha.Banana.Prelude
  ( -- * Terminal
    TermEvent
    -- * Banana
  , Banana(..)
  , Events
  , Behavior
  , MonadMoment
  , (<@)
  , (<@>)
  , accumB
  , accumE
  , executeE
  , filterCoincidentE
  , filterE
  , filterJust
  , leftmostE
  , mergeE
  , previewE
  , reactimate
  , stepper
  , switchB
  , switchE
  , unionWith
  , unions
  , unpairE
  , whenE
    -- * Randomness
  , randomInt
  , randomOneOf
  ) where

import Reactive.Banana            hiding (Event)
import Reactive.Banana.Frameworks hiding (reactimate)
import System.Random
import Termbox.Banana             (Event)

import qualified Reactive.Banana
import qualified Reactive.Banana.Frameworks as Reactive.Banana

import Bha.Prelude

type TermEvent
  = Event

type Events
  = Reactive.Banana.Event

-- | A wrapper around MomentIO, used to control what effects games are allowed
-- to use.
newtype Banana a
  = Banana { unBanana :: MomentIO a }
  deriving newtype (Applicative, Functor, Monad, MonadFix, MonadMoment)

-- TODO IO wrapper

reactimate :: Events (IO ()) -> Banana ()
reactimate =
  Banana . Reactive.Banana.reactimate

executeE :: Events (Banana a) -> Banana (Events a)
executeE e =
  Banana (execute (unBanana <$> e))

filterCoincidentE :: Events b -> Events a -> Events a
filterCoincidentE e1 e2 =
  filterJust (mergeE (const Nothing) (const Nothing) (const Just) e1 e2)

leftmostE :: [Events a] -> Events a
leftmostE =
  foldr (unionWith const) never

data Merge a b
  = MergeL a
  | MergeR b
  | MergeLR a b

mergeE
  :: (a -> c)
  -> (b -> c)
  -> (a -> b -> c)
  -> Events a
  -> Events b
  -> Events c
mergeE f g h xs ys =
  (\case
    MergeL x -> f x
    MergeR y -> g y
    MergeLR x y -> h x y)
  <$>
  unionWith
    (\(MergeL x) (MergeR y) -> MergeLR x y)
    (MergeL <$> xs)
    (MergeR <$> ys)

-- | Filter an 'Event' with a 'Prism''.
previewE :: Prism' s a -> Events s -> Events a
previewE p e =
  filterJust (preview p <$> e)

-- | Split an event of @(a, b)@ into two coindicent events of @a@ and @b@.
unpairE :: Events (a, b) -> (Events a, Events b)
unpairE e =
  (fst <$> e, snd <$> e)

randomInt :: Int -> Int -> Banana Int
randomInt x y =
  Banana (liftIO (randomRIO (x, y)))

randomOneOf :: [a] -> Banana a
randomOneOf [] = error "randomOneOf: []"
randomOneOf xs =
  (xs !!) <$> randomInt 0 (length xs - 1)
