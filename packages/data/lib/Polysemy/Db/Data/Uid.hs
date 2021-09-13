{-# language NoImplicitPrelude #-}

module Polysemy.Db.Data.Uid where

import Control.Comonad (Comonad(..))
import Control.Lens (makeClassy, Lens')
import Data.Aeson (FromJSON(..), ToJSON(..), genericParseJSON, object, withObject, (.:), (.=))
import qualified Data.UUID as UUID
import Data.UUID (UUID)
import Prelude (Eq, Functor, Generic, Int, Show, fromIntegral, (<$>), (<*>), (<|>), Applicative(..), Monad(..))

import Polysemy.Db.Json (jsonOptions)

data Uid i a =
  Uid {
    _id :: i,
    _payload :: a
  }
  deriving (Eq, Show, Generic, Functor)

makeClassy ''Uid

instance Applicative (Uid ()) where
  pure =
    Uid ()
  (<*>) (Uid () f) (Uid () a) =
    Uid () (f a)

instance Monad (Uid ()) where
  (>>=) (Uid () a) f =
    f a

instance (FromJSON a, FromJSON i) => FromJSON (Uid i a) where
  parseJSON v =
    withObject "Uid" parseFlat v <|> genericParseJSON jsonOptions v
    where
      parseFlat o =
        Uid <$> o .: "id" <*> parseJSON v

instance (ToJSON a, ToJSON i) => ToJSON (Uid i a) where
  toJSON (Uid id' a) =
    object ["id" .= toJSON id', "payload" .= a]

instance Comonad (Uid i) where
  extract (Uid _ a) =
    a
  duplicate u@(Uid id' _) =
    Uid id' u

type Uuid =
  Uid UUID

uuid :: Int -> UUID
uuid int =
  UUID.fromWords i' i' i' i'
  where
    i' =
      fromIntegral int

intUuid :: Int -> a -> Uuid a
intUuid i' =
  Uid (uuid i')

uId :: HasUid c i d => Lens' c i
uId =
  id
