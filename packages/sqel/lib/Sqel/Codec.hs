module Sqel.Codec where

import qualified Chronos as Chronos
import Data.Time (Day, DiffTime, LocalTime, TimeOfDay, TimeZone, UTCTime)
import Data.UUID (UUID)
import qualified Hasql.Decoders as Decoders
import Hasql.Decoders (Row)
import qualified Hasql.Encoders as Encoders
import Hasql.Encoders (Params)
import Path (Path)
import Prelude hiding (sum)
import Type.Errors.Pretty (type (<>), type (%))

import qualified Sqel.Codec.PrimDecoder as PrimDecoder
import Sqel.Codec.PrimDecoder (PrimDecoder)
import qualified Sqel.Codec.PrimEncoder as PrimEncoder
import Sqel.Codec.PrimEncoder (PrimEncoder)
import Sqel.Codec.Sum (ignoreEncoder)
import Sqel.Column (ignoreDecoder)
import qualified Sqel.Data.Codec as Codec
import Sqel.Data.Codec (Codec (Codec), Decoder (Decoder), Encoder (Encoder), FullCodec, ValueCodec)
import Sqel.Data.PgType (PgPrimName)
import Sqel.SOP.Error (QuotedType, Quoted)

class ColumnEncoder f where
  columnEncoder :: f a -> Params a
  columnEncoderNullable :: f a -> Params (Maybe a)
  columnEncoderIgnore :: f a -> Params b

instance ColumnEncoder Encoders.Value where
  columnEncoder =
    Encoders.param . Encoders.nonNullable
  columnEncoderNullable =
    Encoders.param . Encoders.nullable
  columnEncoderIgnore =
    ignoreEncoder

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

  pgType :: PgPrimName

instance {-# overlappable #-} (
    TypeError (
      "A column of type " <> QuotedType a <> " was declared as primitive," %
      "but there is no instance of " <> Quoted "PrimColumn" <> " for that type." %
      "If it is a newtype, ensure that it has " <> Quoted "Generic" <> " and use " <> Quoted "primNewtype" <> "."
    )
  ) => PrimColumn a where
    primDecoder = error "no instance for PrimColumn"
    primEncoder = error "no instance for PrimColumn"
    pgType = error "no instance for PrimColumn"

instance PrimColumn Bool where pgType = "bool"
instance PrimColumn Int where pgType = "bigint"
instance PrimColumn Int64 where pgType = "bigint"
instance PrimColumn Double where pgType = "double precision"
instance PrimColumn Text where pgType = "text"
instance PrimColumn ByteString where pgType = "bytes"
instance PrimColumn UUID where pgType = "uuid"
instance PrimColumn Day where pgType = "date"
instance PrimColumn LocalTime where pgType = "timestamp without time zone"
instance PrimColumn UTCTime where pgType = "timestamp with time zone"
instance PrimColumn TimeOfDay where pgType = "time without time zone"
instance PrimColumn (TimeOfDay, TimeZone) where pgType = "time with time zone"
instance PrimColumn DiffTime where pgType = "interval"
instance PrimColumn Chronos.Date where pgType = "date"
instance PrimColumn Chronos.Time where pgType = "bigint"
instance PrimColumn Chronos.Datetime where pgType = "timestamp without time zone"
instance PrimDecoder (Path b t) => PrimColumn (Path b t) where pgType = "text"
instance PrimColumn () where pgType = "bool"

fullPrimCodec ::
  Encoders.Value a ->
  Decoders.Value a ->
  FullCodec a
fullPrimCodec encoder decoder =
  Codec {
    encoder = Encoder (columnEncoder encoder) (columnEncoderIgnore encoder),
    decoder = Decoder (columnDecoder decoder) (void ignoreDecoder)
  }

toFullPrimCodec ::
  ValueCodec a ->
  FullCodec a
toFullPrimCodec (Codec encoder decoder) =
  fullPrimCodec encoder decoder
