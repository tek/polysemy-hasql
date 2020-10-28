module Polysemy.Hasql.Table.ValueDecoder where

import qualified Chronos as Chronos
import Data.Scientific (Scientific)
import Data.Time (
  Day,
  DiffTime,
  LocalTime,
  TimeOfDay,
  TimeZone,
  UTCTime,
  toModifiedJulianDay,
  )
import Hasql.Decoders (
  Value,
  bool,
  bytea,
  char,
  custom,
  date,
  float4,
  float8,
  int2,
  int4,
  int8,
  interval,
  numeric,
  text,
  time,
  timestamp,
  timestamptz,
  timetz,
  uuid,
  )
import Path (Abs, Dir, File, Path, Rel, parseAbsDir, parseAbsFile, parseRelDir, parseRelFile)
import Prelude hiding (Enum, bool)

import Polysemy.Db.Data.Column (Enum, NewtypePrim, Prim)
import Polysemy.Db.SOP.Constraint (NewtypeCoded)
import Polysemy.Hasql.SOP.Enum (EnumTable)
import Polysemy.Hasql.Table.Enum (enumDecodeValue)

class ValueDecoder a where
  valueDecoder :: Value a

instance ValueDecoder Bool where
  valueDecoder =
    bool
  {-# inline valueDecoder #-}

instance ValueDecoder Int16 where
  valueDecoder =
    int2
  {-# inline valueDecoder #-}

instance ValueDecoder Int32 where
  valueDecoder =
    int4
  {-# inline valueDecoder #-}

instance ValueDecoder Int64 where
  valueDecoder =
    int8
  {-# inline valueDecoder #-}

instance ValueDecoder Int where
  valueDecoder =
    fromIntegral <$> int8
  {-# inline valueDecoder #-}

instance ValueDecoder Float where
  valueDecoder =
    float4
  {-# inline valueDecoder #-}

instance ValueDecoder Double where
  valueDecoder =
    float8
  {-# inline valueDecoder #-}

instance ValueDecoder Scientific where
  valueDecoder =
    numeric
  {-# inline valueDecoder #-}

instance ValueDecoder Char where
  valueDecoder =
    char
  {-# inline valueDecoder #-}

instance ValueDecoder Text where
  valueDecoder =
    text
  {-# inline valueDecoder #-}

instance ValueDecoder ByteString where
  valueDecoder =
    bytea
  {-# inline valueDecoder #-}

instance ValueDecoder Day where
  valueDecoder =
    date
  {-# inline valueDecoder #-}

instance ValueDecoder LocalTime where
  valueDecoder =
    timestamp
  {-# inline valueDecoder #-}

instance ValueDecoder UTCTime where
  valueDecoder =
    timestamptz
  {-# inline valueDecoder #-}

instance ValueDecoder TimeOfDay where
  valueDecoder =
    time
  {-# inline valueDecoder #-}

instance ValueDecoder (TimeOfDay, TimeZone) where
  valueDecoder =
    timetz
  {-# inline valueDecoder #-}

instance ValueDecoder DiffTime where
  valueDecoder =
    interval
  {-# inline valueDecoder #-}

instance ValueDecoder UUID where
  valueDecoder =
    uuid
  {-# inline valueDecoder #-}

decodePath ::
  Show e =>
  (String -> Either e (Path b t)) ->
  Bool ->
  ByteString ->
  Either Text (Path b t)
decodePath parse _ =
  mapLeft show . parse . decodeUtf8

instance ValueDecoder (Path Abs File) where
  valueDecoder =
    custom (decodePath parseAbsFile)
  {-# inline valueDecoder #-}

instance ValueDecoder (Path Abs Dir) where
  valueDecoder =
    custom (decodePath parseAbsDir)
  {-# inline valueDecoder #-}

instance ValueDecoder (Path Rel File) where
  valueDecoder =
    custom (decodePath parseRelFile)
  {-# inline valueDecoder #-}

instance ValueDecoder (Path Rel Dir) where
  valueDecoder =
    custom (decodePath parseRelDir)
  {-# inline valueDecoder #-}

instance ValueDecoder Chronos.Date where
  valueDecoder =
    Chronos.dayToDate . Chronos.Day . fromIntegral . toModifiedJulianDay <$> date
  {-# inline valueDecoder #-}

instance ValueDecoder Chronos.Time where
  valueDecoder =
    Chronos.Time <$> int8
  {-# inline valueDecoder #-}

instance ValueDecoder Chronos.Datetime where
  valueDecoder =
    Chronos.timeToDatetime <$> valueDecoder
  {-# inline valueDecoder #-}

class RepDecoder (rep :: *) (a :: *) where
  repDecoder :: Value a

instance {-# overlappable #-} ValueDecoder a => RepDecoder r a where
  repDecoder =
    valueDecoder

instance ValueDecoder a => RepDecoder (Prim r) a where
  repDecoder =
    valueDecoder

instance EnumTable a => RepDecoder (Enum r) a where
  repDecoder =
    enumDecodeValue

instance (NewtypeCoded a c, ValueDecoder c) => RepDecoder (NewtypePrim r) a where
  repDecoder =
    coerce <$> valueDecoder @c
