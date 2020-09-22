module Polysemy.Db.Data.Uid where

import Data.Aeson (Value(Object), genericParseJSON, object, withObject, (.:), (.=))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.UUID as UUID
import Prelude hiding (id, (.:))

import Control.Comonad (Comonad(..))

data Uid i a =
  Uid {
    _id :: i,
    _payload :: a
  }
  deriving (Eq, Show, Generic, Functor)

makeClassy ''Uid

instance (FromJSON a, FromJSON i) => FromJSON (Uid i a) where
  parseJSON v =
    withObject "Uid" parseFlat v <|> genericParseJSON jsonOptions v
    where
      parseFlat o =
        Uid <$> o .: "id" <*> parseJSON v

instance (ToJSON a, ToJSON i) => ToJSON (Uid i a) where
  toJSON (Uid id' a) =
    case toJSON a of
      Object aObject -> Object (aObject <> HashMap.fromList [("id", toJSON id')])
      aValue -> object ["id" .= toJSON id', "payload" .= aValue]

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
