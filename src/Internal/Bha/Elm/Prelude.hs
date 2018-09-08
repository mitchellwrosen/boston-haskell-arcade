module Internal.Bha.Elm.Prelude
  ( ElmGame(..)
  , ElmF(..)
  , Init(..)
  , runInit
  , Update(..)
  , runUpdate
  , MonadElm(..)
  , gameover
  , randomInt
  , randomPct
  ) where

import Control.Monad.State
import Control.Monad.Trans.Free
import Data.Functor.Identity
import Termbox.Banana           (Event, Scene)

import Bha.Prelude

class Monad m => MonadElm m where
  interpretElm :: ElmF a -> m a

instance MonadElm Init where
  interpretElm :: ElmF a -> Init a
  interpretElm =
    Init . liftF

instance MonadElm (Update s) where
  interpretElm :: ElmF a -> Update s a
  interpretElm =
    Update . lift . liftF

newtype Init a
  = Init { unInit :: Free ElmF a }
  deriving newtype (Applicative, Functor, Monad)

runInit :: Monad m => (forall x. ElmF (m x) -> m x) -> Init a -> m a
runInit phi =
  iterT phi . hoistFreeT (pure . runIdentity) . unInit

newtype Update s a
  = Update { unUpdate :: StateT s (FreeT ElmF Maybe) a }
  deriving newtype (Alternative, Applicative, Functor, Monad, MonadState s)

runUpdate
  :: forall a m s.
     Monad m
  => s
  -> (forall x. ElmF (MaybeT m x) -> MaybeT m x)
  -> Update s a
  -> m (Maybe (a, s))
runUpdate s phi (Update m) =
  runMaybeT (iterT phi (hoistFreeT (MaybeT . pure) (runStateT m s)))

data ElmF x
  = Save !Text !ByteString x
  | Load !Text (Maybe ByteString -> x)
  | RandomInt !Int !Int (Int -> x)
  | RandomPct (Double -> x)
  deriving Functor

-- | An Elm-style game.
data ElmGame model
  = ElmGame
      (Init model)
      -- Initial model.
      (Either NominalDiffTime Event -> Update model ())
      -- Update the model from a tick or terminal event.
      (model -> Scene)
      -- Render the model.
      (model -> Maybe NominalDiffTime)
      -- Tick, and if so, how often?

gameover :: Update s a
gameover =
  empty

-- | Generate a random 'Int' in the given bounds (inclusive).
randomInt :: MonadElm m => Int -> Int -> m Int
randomInt lo hi =
  interpretElm (RandomInt lo hi id)

randomPct :: MonadElm m => m Double
randomPct =
  interpretElm (RandomPct id)
