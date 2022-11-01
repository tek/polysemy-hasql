module Sqel.Data.Uid where

import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, object, withObject, (.:), (.=))
import qualified Data.UUID as UUID
import Data.UUID (UUID)

data Uid i a =
  Uid {
    id :: i,
    payload :: a
  }
  deriving stock (Eq, Show, Generic, Functor)

instance (FromJSON a, FromJSON i) => FromJSON (Uid i a) where
  parseJSON v =
    withObject "Uid" parseFlat v <|> genericParseJSON Aeson.defaultOptions v
    where
      parseFlat o =
        Uid <$> o .: "id" <*> parseJSON v

instance (ToJSON a, ToJSON i) => ToJSON (Uid i a) where
  toJSON (Uid id' a) =
    object ["id" .= toJSON id', "payload" .= a]

type Uuid =
  Uid UUID

intUUID :: Int -> UUID
intUUID int =
  UUID.fromWords i' i' i' i'
  where
    i' =
      fromIntegral int

intUuid :: Int -> a -> Uuid a
intUuid i' =
  Uid (intUUID i')
