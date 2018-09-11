module Internal.Bha.Elm.Prelude
  ( ElmGame(..)
  , Input(..)
  , ElmF(..)
  , Init(..)
  , runInit
  , Update(..)
  , runUpdate
  , MonadElm(..)
  , gameover
  , randomInt
  , randomPct
  , send
  ) where

import Control.Monad.State
import Control.Monad.Trans.Free
import Data.Functor.Identity
import Termbox.Banana           (Key, Mouse)

import Bha.Prelude
import Internal.Bha.View (Scene)

--------------------------------------------------------------------------------
-- Elm game
--------------------------------------------------------------------------------

-- | An Elm-style game.
data ElmGame model message
  = ElmGame
      (Init message model)
      -- Initial model.
      (Input message -> Update model message ())
      -- Update the model from a tick or terminal event.
      (model -> Scene)
      -- Render the model.
      (model -> Maybe NominalDiffTime)
      -- Tick, and if so, how often?
      (model -> HashSet Text)


--------------------------------------------------------------------------------
-- Init and Update monads
--------------------------------------------------------------------------------

newtype Init message a
  = Init { unInit :: Free (ElmF message) a }
  deriving newtype (Applicative, Functor, Monad)

runInit
  :: Monad m
  => (forall x. ElmF message (m x) -> m x)
  -> Init message a
  -> m a
runInit phi =
  iterT phi . hoistFreeT (pure . runIdentity) . unInit


newtype Update model message a
  = Update { unUpdate :: StateT model (FreeT (ElmF message) Maybe) a }
  deriving newtype (Alternative, Applicative, Functor, Monad, MonadState model)

runUpdate
  :: forall a m message model.
     Monad m
  => model
  -> (forall x. ElmF message (MaybeT m x) -> MaybeT m x)
  -> Update model message a
  -> m (Maybe (a, model))
runUpdate s phi (Update m) =
  runMaybeT (iterT phi (hoistFreeT (MaybeT . pure) (runStateT m s)))


class Monad m => MonadElm message m | m -> message where
  interpretElm :: ElmF message a -> m a

instance MonadElm message (Init message) where
  interpretElm :: ElmF message a -> Init message a
  interpretElm =
    Init . liftF

instance MonadElm message (Update model message) where
  interpretElm :: ElmF message a -> Update model message a
  interpretElm =
    Update . lift . liftF


data ElmF message x
  = Save !Text !ByteString x
  | Load !Text (Maybe ByteString -> x)
  | RandomInt !Int !Int (Int -> x)
  | RandomPct (Double -> x)
  | Send !Text !message x
  deriving (Functor)


gameover :: Update model message a
gameover =
  empty

-- | Generate a random 'Int' in the given bounds (inclusive).
randomInt :: MonadElm message m => Int -> Int -> m Int
randomInt lo hi =
  interpretElm (RandomInt lo hi id)

randomPct :: MonadElm message m => m Double
randomPct =
  interpretElm (RandomPct id)


send :: MonadElm message m => Text -> message -> m ()
send topic message =
  interpretElm (Send topic message ())


--------------------------------------------------------------------------------
-- Input
--------------------------------------------------------------------------------

data Input a
  = Key !Key
  | Mouse !Mouse !Int !Int -- Col, then row
  | Resize !Int !Int -- Col, then row
  | Tick !NominalDiffTime
  | Message !Text !a
