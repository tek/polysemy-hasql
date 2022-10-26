module Polysemy.Hasql.Table.Dsl.Codec where

import qualified Hasql.Decoders as Decoders
import Hasql.Decoders (Row)
import qualified Hasql.Encoders as Encoders
import Hasql.Encoders (Params)
import Prelude hiding (sum)

import Polysemy.Hasql.Table.Dsl.Data.PgColumn (PgType)
import qualified Polysemy.Hasql.Table.PrimDecoder as PrimDecoder
import Polysemy.Hasql.Table.PrimDecoder (PrimDecoder)
import qualified Polysemy.Hasql.Table.PrimEncoder as PrimEncoder
import Polysemy.Hasql.Table.PrimEncoder (PrimEncoder)

class ColumnEncoder f where
  columnEncoder :: f a -> Params a
  columnEncoderNullable :: f a -> Params (Maybe a)

instance ColumnEncoder Encoders.Value where
  columnEncoder =
    Encoders.param . Encoders.nonNullable
  columnEncoderNullable =
    Encoders.param . Encoders.nullable

class ColumnDecoder f where
  columnDecoder :: f a -> Row a
  columnDecoderNullable :: f a -> Row (Maybe a)

instance ColumnDecoder Decoders.Value where
  columnDecoder =
    Decoders.column . Decoders.nonNullable
  columnDecoderNullable =
    Decoders.column . Decoders.nullable

class PrimColumn a where
  primDecoder :: Decoders.Value a
  default primDecoder :: PrimDecoder a => Decoders.Value a
  primDecoder = PrimDecoder.primDecoder

  primEncoder :: Encoders.Value a
  default primEncoder :: PrimEncoder a => Encoders.Value a
  primEncoder = PrimEncoder.primEncoder

  pgType :: PgType a

instance PrimColumn Int where
  pgType = "bigint"

instance PrimColumn Int64 where
  pgType = "bigint"

instance PrimColumn Text where
  pgType = "text"
