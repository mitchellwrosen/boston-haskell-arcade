module Internal.Bha.Banana.Prelude
  ( -- * Terminal
    TermEvent
    -- * Banana
  , Banana(..)
  , runBanana
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
  , mapMaybeE
  , mergeE
  , never
  , previewE
  , stepper
  , switchB
  , switchE
  , unionWith
  , unions
  , unpairE
  , whenE
    -- * Randomness
  , randomBool
  , randomInt
  , randomOneOf
  , randomPct
  ) where

import Control.Monad.Reader

import Reactive.Banana            hiding (Event)
import Reactive.Banana.Frameworks
import System.Random
import Termbox.Banana             (Event)

import qualified Reactive.Banana

import Bha.Prelude

type TermEvent
  = Event

type Events
  = Reactive.Banana.Event

-- | A wrapper around MomentIO, used to control what effects games are allowed
-- to use.
newtype Banana a
  = Banana { unBanana :: ReaderT [Char] MomentIO a }
  deriving newtype (Applicative, Functor, Monad, MonadFix)

instance MonadMoment Banana where
  liftMoment :: Moment a -> Banana a
  liftMoment =
    Banana . lift . liftMoment

runBanana :: Banana a -> [Char] -> MomentIO a
runBanana =
  runReaderT . unBanana

executeE :: Events (Banana a) -> Banana (Events a)
executeE e =
  Banana $ ReaderT $ \name -> do
    execute ((`runBanana` name) <$> e)

filterCoincidentE :: Events b -> Events a -> Events a
filterCoincidentE e1 e2 =
  filterJust (mergeE (const Nothing) (const Nothing) (const Just) e1 e2)

leftmostE :: [Events a] -> Events a
leftmostE =
  foldr (unionWith const) never

mapMaybeE :: (a -> Maybe b) -> Events a -> Events b
mapMaybeE f x =
  filterJust (f <$> x)

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
  resolve <$> unionWith both (MergeL <$> xs) (MergeR <$> ys)
 where
  resolve :: Merge a b -> c
  resolve = \case
    MergeL x -> f x
    MergeR y -> g y
    MergeLR x y -> h x y

  both :: Merge a b -> Merge a b -> Merge a b
  both (MergeL x) (MergeR y) = MergeLR x y
  both _ _ = undefined

-- | Filter an 'Event' with a 'Prism''.
previewE :: Prism' s a -> Events s -> Events a
previewE p e =
  filterJust (preview p <$> e)

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
  (xs !!) <$> randomInt 0 (length xs - 1)

randomPct :: Banana Double
randomPct =
  Banana (liftIO randomIO)
