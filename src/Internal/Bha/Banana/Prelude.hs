module Internal.Bha.Banana.Prelude
  ( -- * Terminal
    TermEvent
    -- * Banana
  , Banana(..)
  , runBanana
  , Events
  , FRP.Behavior
  , FRP.MonadMoment
  , (FRP.<@)
  , (FRP.<@>)
  , FRP.accumB
  , FRP.accumE
  , executeE
  , filterCoincidentE
  , FRP.filterE
  , FRP.filterJust
  , leftmostE
  , mapMaybeE
  , mergeE
  , FRP.never
  , previewE
  , FRP.stepper
  , FRP.switchB
  , FRP.switchE
  , FRP.unionWith
  , FRP.unions
  , unpairE
  , FRP.whenE
    -- * Randomness
  , randomBool
  , randomInt
  , randomOneOf
  , randomPct
  ) where

import Monad.Fix
import Optic.Fold.Unsafe ((^?!))
import Optic.Traversal   (ix)
import Reader
import System.Random
import Termbox.Banana    (Event)

import qualified FRP

import Bha.Prelude

type TermEvent
  = Event

type Events
  = FRP.Event

-- | A wrapper around MomentIO, used to control what effects games are allowed
-- to use.
newtype Banana a
  = Banana { unBanana :: ReaderT [Char] FRP.MomentIO a }
  deriving newtype (Applicative, Functor, Monad, MonadFix)

instance FRP.MonadMoment Banana where
  liftMoment :: FRP.Moment a -> Banana a
  liftMoment =
    Banana . lift . FRP.liftMoment

runBanana :: Banana a -> [Char] -> FRP.MomentIO a
runBanana =
  runReaderT . unBanana

executeE :: Events (Banana a) -> Banana (Events a)
executeE e =
  Banana $ ReaderT $ \name -> do
    FRP.execute ((`runBanana` name) <$> e)

filterCoincidentE :: Events b -> Events a -> Events a
filterCoincidentE e1 e2 =
  FRP.filterJust (mergeE (const Nothing) (const Nothing) (const Just) e1 e2)

leftmostE :: [Events a] -> Events a
leftmostE =
  foldr (FRP.unionWith const) FRP.never

mapMaybeE :: (a -> Maybe b) -> Events a -> Events b
mapMaybeE f x =
  FRP.filterJust (f <$> x)

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
  resolve <$> FRP.unionWith both (MergeL <$> xs) (MergeR <$> ys)
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
  FRP.filterJust (preview p <$> e)

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
