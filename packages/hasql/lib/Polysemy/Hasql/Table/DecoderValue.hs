module Polysemy.Hasql.Table.DecoderValue where

import Hasql.Decoders (Value, jsonBytes)
import Polysemy.Db.Data.Column (Enum, Json)
import Prelude hiding (Enum, bool)

import qualified Data.Aeson as Aeson
import Polysemy.Db.Data.Column (Prim)
import Polysemy.Hasql.SOP.Enum (EnumTable)
import Polysemy.Hasql.Table.Enum (enumDecodeValue)
import Polysemy.Hasql.Table.PrimDecoder (PrimDecoder, primDecoder)
import Polysemy.Db.Tree.Data.Effect (Newtype)

class DecoderValue (effs :: [*]) (a :: *) where
  decoderValue :: Value a

instance {-# overlappable #-} DecoderValue effs d => DecoderValue (eff : effs) d where
  decoderValue =
    decoderValue @effs

instance {-# overlappable #-} PrimDecoder a => DecoderValue effs a where
  decoderValue =
    primDecoder

instance PrimDecoder a => DecoderValue '[Prim] a where
  decoderValue =
    primDecoder

instance EnumTable a => DecoderValue '[Enum] a where
  decoderValue =
    enumDecodeValue

-- instance EnumTable a => DecoderValue '[ADT meta Enum] a where
--   decoderValue =
--     enumDecodeValue

instance FromJSON a => DecoderValue '[Json] a where
  decoderValue =
    jsonBytes (mapLeft toText . Aeson.eitherDecodeStrict')

instance (
    Coercible n d,
    DecoderValue effs d
  ) => DecoderValue (Newtype n d : effs) n where
  decoderValue =
    coerce <$> decoderValue @effs @d
