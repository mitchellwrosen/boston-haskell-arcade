module Internal.Bha.Banana.Prelude
  ( -- * Terminal
    TermEvent
    -- * Banana
  , Banana(..)
  , runBanana
  , Events
  , Reactive.Banana.Behavior
  , Reactive.Banana.MonadMoment
  , (Reactive.Banana.<@)
  , (Reactive.Banana.<@>)
  , Reactive.Banana.accumB
  , Reactive.Banana.accumE
  , executeE
  , filterCoincidentE
  , Reactive.Banana.filterE
  , Reactive.Banana.filterJust
  , leftmostE
  , mapMaybeE
  , mergeE
  , Reactive.Banana.never
  , previewE
  , Reactive.Banana.stepper
  , Reactive.Banana.switchB
  , Reactive.Banana.switchE
  , Reactive.Banana.unionWith
  , Reactive.Banana.unions
  , unpairE
  , Reactive.Banana.valueB
  , Reactive.Banana.whenE
    -- * Randomness
  , randomBool
  , randomInt
  , randomOneOf
  , randomPct
  ) where

import Bha.Prelude

import Control.Lens         ((^?!))
import Control.Monad.Fix
import Control.Monad.Reader
import System.Random
import Termbox.Banana       (Event)

import qualified Reactive.Banana
import qualified Reactive.Banana.Frameworks as Reactive.Banana


type TermEvent
  = Event

type Events
  = Reactive.Banana.Event

-- | A wrapper around MomentIO, used to control what effects games are allowed
-- to use.
newtype Banana a
  = Banana { unBanana :: ReaderT [Char] Reactive.Banana.MomentIO a }
  deriving newtype (Applicative, Functor, Monad, MonadFix)

instance Reactive.Banana.MonadMoment Banana where
  liftMoment :: Reactive.Banana.Moment a -> Banana a
  liftMoment =
    Banana . lift . Reactive.Banana.liftMoment

runBanana :: Banana a -> [Char] -> Reactive.Banana.MomentIO a
runBanana =
  runReaderT . unBanana

executeE :: Events (Banana a) -> Banana (Events a)
executeE e =
  Banana $ ReaderT $ \name -> do
    Reactive.Banana.execute ((`runBanana` name) <$> e)

filterCoincidentE :: Events b -> Events a -> Events a
filterCoincidentE e1 e2 =
  Reactive.Banana.filterJust (mergeE (const Nothing) (const Nothing) (const Just) e1 e2)

leftmostE :: [Events a] -> Events a
leftmostE =
  foldr (Reactive.Banana.unionWith const) Reactive.Banana.never

mapMaybeE :: (a -> Maybe b) -> Events a -> Events b
mapMaybeE f x =
  Reactive.Banana.filterJust (f <$> x)

data Merge a b
  = MergeL a
  | MergeR b
  | MergeLR a b

mergeE
  :: forall a b c.
     (a -> c)
  -> (b -> c)
  -> (a -> b -> c)
  -> Events a
  -> Events b
  -> Events c
mergeE f g h xs ys =
  resolve <$> Reactive.Banana.unionWith both (MergeL <$> xs) (MergeR <$> ys)
  where
    resolve :: Merge a b -> c
    resolve = \case
      MergeL x -> f x
      MergeR y -> g y
      MergeLR x y -> h x y

    both :: Merge a b -> Merge a b -> Merge a b
    both (MergeL x) (MergeR y) = MergeLR x y
    both _ _                   = undefined

-- | Filter an 'Event' with a 'Prism''.
previewE :: Prism' s a -> Events s -> Events a
previewE p e =
  Reactive.Banana.filterJust (preview p <$> e)

-- | Split an event of @(a, b)@ into two coindicent events of @a@ and @b@.
unpairE :: Events (a, b) -> (Events a, Events b)
unpairE e =
  (fst <$> e, snd <$> e)

randomBool :: Banana Bool
randomBool =
  Banana (liftIO randomIO)

randomInt :: Int -> Int -> Banana Int
randomInt x y =
  Banana (liftIO (randomRIO (x, y)))

randomOneOf :: [a] -> Banana a
randomOneOf [] = error "randomOneOf: []"
randomOneOf xs =
  (\i -> xs ^?! ix i) <$> randomInt 0 (length xs - 1)

randomPct :: Banana Double
randomPct =
  Banana (liftIO randomIO)
