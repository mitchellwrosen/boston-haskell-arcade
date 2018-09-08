{-# LANGUAGE UndecidableInstances #-}

module Internal.Bha.Versioned
  ( Versioned
  , encodeVersioned
  , decodeVersioned
  , Migrate(..)
  , Migrations
  ) where

import Data.Serialize
import GHC.Generics (Rep)
import GHC.TypeLits

import qualified GHC.Generics

import Bha.Prelude

class
  ( Generic a
  , KnownNat (Length as)
  , Migrations as a
  , Serialize a
  ) => Versioned (as :: [Type]) (a :: Type) | a -> as

encodeVersioned :: forall a as. Versioned as a => a -> ByteString
encodeVersioned x =
  runPut $ do
    putWord8 (fromIntegral (natVal @(Length as) Proxy))
    put x

decodeVersioned :: forall as a. Versioned as a => ByteString -> Either String a
decodeVersioned =
  runGet $ do
    ver <- getWord8

    case migrations @as Proxy ^? ix (fromIntegral ver) of
      Nothing ->
        fail $ "Expected version " ++ show (natVal @(Length as) Proxy) ++
          " or earlier, but found ver " ++ show ver

      Just f ->
        f

class Migrations (as :: [Type]) x where
  migrations :: Proxy as -> [Get x]

instance (Generic x, GSerializeGet (Rep x)) => Migrations '[] x where
  migrations _ =
    [GHC.Generics.to <$> gGet]

instance (Serialize a, Migrate a x, Migrations as x) => Migrations (a ': as) x where
  migrations _ =
    (migrate @a <$> get) : migrations @as Proxy

class Migrate a b where
  migrate :: a -> b

type family Length (xs :: [a]) :: Nat where
  Length '[] = 0
  Length (_ ': xs) = 1 + Length xs
