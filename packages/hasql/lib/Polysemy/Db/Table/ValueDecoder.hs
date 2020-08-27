{-# LANGUAGE UndecidableSuperClasses #-}

module Polysemy.Db.Table.ValueDecoder where

import qualified Chronos as Chronos
import qualified Data.Map.Strict as Map
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
import Generics.SOP (Generic)
import Generics.SOP.Universe (Code)
import Hasql.Decoders (
  Value,
  bool,
  bytea,
  char,
  custom,
  date,
  enum,
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
import Prelude hiding (Generic, bool)

import Polysemy.Db.SOP.Enum (EnumTable(enumTable))

class ValueDecoder a where
  valueDecoder :: Value a

class (Generic a, b ~ Code a) => GenDecoder a b where
  genDecoder :: Value a

enumDecoder ::
  EnumTable a =>
  Value a
enumDecoder =
  enum (`Map.lookup` enumTable)

-- necessary to disambiguate the enum instances from the newtype instance
instance (EnumTable a, Generic a, Code a ~ '[ '[] ]) => GenDecoder a '[ '[] ] where
  genDecoder =
    enumDecoder

instance (EnumTable a, Generic a, Code a ~ ('[] : cs)) => GenDecoder a ('[] : cs) where
  genDecoder =
    enumDecoder

instance (Coercible c a, Generic a, Code a ~ '[ '[c]], ValueDecoder c) => GenDecoder a '[ '[c]] where
  genDecoder =
    coerce <$> valueDecoder @c

instance
  {-# OVERLAPPABLE #-}
  (Generic a, GenDecoder a b) => ValueDecoder a where
  valueDecoder =
    genDecoder @a

instance ValueDecoder Bool where
  valueDecoder =
    bool
  {-# INLINE valueDecoder #-}

instance ValueDecoder Int16 where
  valueDecoder =
    int2
  {-# INLINE valueDecoder #-}

instance ValueDecoder Int32 where
  valueDecoder =
    int4
  {-# INLINE valueDecoder #-}

instance ValueDecoder Int64 where
  valueDecoder =
    int8
  {-# INLINE valueDecoder #-}

instance ValueDecoder Int where
  valueDecoder =
    fromIntegral <$> int8
  {-# INLINE valueDecoder #-}

instance ValueDecoder Float where
  valueDecoder =
    float4
  {-# INLINE valueDecoder #-}

instance ValueDecoder Double where
  valueDecoder =
    float8
  {-# INLINE valueDecoder #-}

instance ValueDecoder Scientific where
  valueDecoder =
    numeric
  {-# INLINE valueDecoder #-}

instance ValueDecoder Char where
  valueDecoder =
    char
  {-# INLINE valueDecoder #-}

instance ValueDecoder Text where
  valueDecoder =
    text
  {-# INLINE valueDecoder #-}

instance ValueDecoder ByteString where
  valueDecoder =
    bytea
  {-# INLINE valueDecoder #-}

instance ValueDecoder Day where
  valueDecoder =
    date
  {-# INLINE valueDecoder #-}

instance ValueDecoder LocalTime where
  valueDecoder =
    timestamp
  {-# INLINE valueDecoder #-}

instance ValueDecoder UTCTime where
  valueDecoder =
    timestamptz
  {-# INLINE valueDecoder #-}

instance ValueDecoder TimeOfDay where
  valueDecoder =
    time
  {-# INLINE valueDecoder #-}

instance ValueDecoder (TimeOfDay, TimeZone) where
  valueDecoder =
    timetz
  {-# INLINE valueDecoder #-}

instance ValueDecoder DiffTime where
  valueDecoder =
    interval
  {-# INLINE valueDecoder #-}

instance ValueDecoder UUID where
  valueDecoder =
    uuid
  {-# INLINE valueDecoder #-}

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
  {-# INLINE valueDecoder #-}

instance ValueDecoder (Path Abs Dir) where
  valueDecoder =
    custom (decodePath parseAbsDir)
  {-# INLINE valueDecoder #-}

instance ValueDecoder (Path Rel File) where
  valueDecoder =
    custom (decodePath parseRelFile)
  {-# INLINE valueDecoder #-}

instance ValueDecoder (Path Rel Dir) where
  valueDecoder =
    custom (decodePath parseRelDir)
  {-# INLINE valueDecoder #-}

instance ValueDecoder Chronos.Date where
  valueDecoder =
    Chronos.dayToDate . Chronos.Day . fromIntegral . toModifiedJulianDay <$> date
  {-# INLINE valueDecoder #-}

instance ValueDecoder Chronos.Time where
  valueDecoder =
    Chronos.Time <$> int8
  {-# INLINE valueDecoder #-}
