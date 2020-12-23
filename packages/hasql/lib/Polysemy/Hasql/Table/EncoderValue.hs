module Polysemy.Hasql.Table.EncoderValue where

import Hasql.Encoders (
  Value,
  enum,
  jsonBytes,
  )
import Polysemy.Db.Data.Column (Json)
import Prelude hiding (Enum, bool)

import qualified Data.Aeson as Aeson
import Polysemy.Db.Data.Column (Enum, Prim)
import Polysemy.Hasql.Table.PrimEncoder (PrimEncoder, primEncoder)
import Polysemy.Hasql.Column.Data.Effect (Newtype)

class EncoderValue (effs :: [*]) (d :: *) where
  encoderValue :: Value d

instance {-# overlappable #-} EncoderValue effs d => EncoderValue (eff : effs) d where
  encoderValue =
    encoderValue @effs

instance {-# overlappable #-} PrimEncoder d => EncoderValue '[Prim] d where
  encoderValue =
    primEncoder

-- instance Show d => EncoderValue '[ADT meta Enum] d where
--   encoderValue =
--     enum show

instance Show d => EncoderValue '[Enum] d where
  encoderValue =
    enum show

instance ToJSON d => EncoderValue '[Json] d where
  encoderValue =
    toStrict . Aeson.encode >$< jsonBytes

instance (
    Coercible n d,
    EncoderValue effs d
  ) => EncoderValue (Newtype n d : effs) n where
  encoderValue =
    coerce (encoderValue @effs @d)
