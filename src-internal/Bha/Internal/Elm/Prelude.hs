module Bha.Internal.Elm.Prelude
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
  , randomBool
  , randomPct
  , send
  ) where

import Bha.Internal.Prelude
import Bha.Internal.View    (Scene)

import Control.Monad.State.Strict (MonadState, StateT, execStateT)
import Control.Monad.Trans.Free   (Free, FreeT, hoistFreeT, iterT, liftF)
import Data.Functor.Identity
import Termbox.Banana             (Key, Mouse)


--------------------------------------------------------------------------------
-- Elm game
--------------------------------------------------------------------------------

-- | An Elm-style game.
data ElmGame model message
  = ElmGame
  { elmInit :: Int -> Int -> Init message model -- ^ Initial model.
  , elmUpdate :: Input message -> Update model message () -- ^ Update the model from a tick or terminal event.
  , elmView :: model -> Scene -- ^ Render the model.
  , elmTickEvery :: model -> Maybe Seconds -- ^ Tick, and if so, how often?
  , elmSubscribe :: model -> HashSet Text -- ^ Set of channels subscribed to.
  }


--------------------------------------------------------------------------------
-- Init and Update monads
--------------------------------------------------------------------------------

newtype Init message a
  = Init { unInit :: Free (ElmF message) a }
  deriving newtype (Applicative, Functor, Monad)

runInit
  :: Monad m
  => Int
  -> Int
  -> (forall x. ElmF message (m x) -> m x)
  -> (Int -> Int -> Init message a)
  -> m a
runInit width height phi action =
  iterT phi (hoistFreeT (pure . runIdentity) (unInit (action width height)))


newtype Update model message a
  = Update { unUpdate :: StateT model (FreeT (ElmF message) Maybe) a }
  deriving newtype (Alternative, Applicative, Functor, Monad, MonadState model)

runUpdate
  :: forall m message model.
     Monad m
  => model
  -> (forall x. ElmF message (MaybeT m x) -> MaybeT m x)
  -> Update model message ()
  -> m (Maybe model)
runUpdate s phi (Update m) =
  runMaybeT (iterT phi (hoistFreeT (MaybeT . pure) (execStateT m s)))


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
  = Save Text ByteString x
  | Load Text (Maybe ByteString -> x)
  | RandomInt Int Int (Int -> x)
  | RandomPct (Double -> x)
  | Send Text message x
  deriving (Functor)


gameover :: Update model message a
gameover =
  empty

-- | Generate a random 'Int' in the given bounds (inclusive).
randomInt :: MonadElm message m => Int -> Int -> m Int
randomInt lo hi =
  interpretElm (RandomInt lo hi id)

randomBool :: MonadElm message m => m Bool
randomBool = 
  randomInt 0 1 >>= \case
    0 -> pure False
    _ -> pure True

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
  = Key Key
  | Mouse Mouse Int Int -- ^ Column, then row
  | Resize Int Int -- ^ The terminal was resized (column, then row)
  | Tick Seconds
  | Message Text a
  deriving (Eq, Show)
