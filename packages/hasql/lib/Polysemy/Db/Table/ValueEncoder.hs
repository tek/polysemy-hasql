module Polysemy.Db.Table.ValueEncoder where

import qualified Chronos as Chronos
import Data.Scientific (Scientific)
import Data.Time (
  Day(ModifiedJulianDay),
  DiffTime,
  LocalTime,
  TimeOfDay,
  TimeZone,
  UTCTime,
  )
import Generics.SOP.Universe (Code)
import Hasql.Encoders
import Path (Path, toFilePath)
import Prelude hiding (bool)

class ValueEncoder a where
  valueEncoder :: Value a

class (Generic a, b ~ Code a) => GenEncoder a b where
  genEncoder :: Value a

enumEncoder ::
  Show a =>
  Value a
enumEncoder =
  enum show

-- necessary to disambiguate the enum instances from the newtype instance
instance (Show a, Generic a, Code a ~ '[ '[] ]) => GenEncoder a '[ '[] ] where
  genEncoder =
    enumEncoder

instance {-# OVERLAPPABLE #-} (Show a, Generic a, Code a ~ ('[] : cs)) => GenEncoder a ('[] : cs) where
  genEncoder =
    enumEncoder

instance (Coercible c a, Generic a, Code a ~ '[ '[c]], ValueEncoder c) => GenEncoder a '[ '[c]] where
  genEncoder =
    coerce >$< valueEncoder @c

instance {-# OVERLAPPABLE #-} GenEncoder a b => ValueEncoder a where
  valueEncoder =
    genEncoder @a

instance ValueEncoder Bool where
  valueEncoder =
    bool
  {-# INLINE valueEncoder #-}

instance ValueEncoder Int16 where
  valueEncoder =
    int2
  {-# INLINE valueEncoder #-}

instance ValueEncoder Int32 where
  valueEncoder =
    int4
  {-# INLINE valueEncoder #-}

instance ValueEncoder Int64 where
  valueEncoder =
    int8
  {-# INLINE valueEncoder #-}

instance ValueEncoder Int where
  valueEncoder =
    contramap fromIntegral int8
  {-# INLINE valueEncoder #-}

instance ValueEncoder Float where
  valueEncoder =
    float4
  {-# INLINE valueEncoder #-}

instance ValueEncoder Double where
  valueEncoder =
    float8
  {-# INLINE valueEncoder #-}

instance ValueEncoder Scientific where
  valueEncoder =
    numeric
  {-# INLINE valueEncoder #-}

instance ValueEncoder Char where
  valueEncoder =
    char
  {-# INLINE valueEncoder #-}

instance ValueEncoder Text where
  valueEncoder =
    text
  {-# INLINE valueEncoder #-}

instance ValueEncoder ByteString where
  valueEncoder =
    bytea
  {-# INLINE valueEncoder #-}

instance ValueEncoder Day where
  valueEncoder =
    date
  {-# INLINE valueEncoder #-}

instance ValueEncoder LocalTime where
  valueEncoder =
    timestamp
  {-# INLINE valueEncoder #-}

instance ValueEncoder UTCTime where
  valueEncoder =
    timestamptz
  {-# INLINE valueEncoder #-}

instance ValueEncoder TimeOfDay where
  valueEncoder =
    time
  {-# INLINE valueEncoder #-}

instance ValueEncoder (TimeOfDay, TimeZone) where
  valueEncoder =
    timetz
  {-# INLINE valueEncoder #-}

instance ValueEncoder DiffTime where
  valueEncoder =
    interval
  {-# INLINE valueEncoder #-}

instance ValueEncoder UUID where
  valueEncoder =
    uuid
  {-# INLINE valueEncoder #-}

instance ValueEncoder (Path b t) where
  valueEncoder =
    (toText . toFilePath) >$< text
  {-# INLINE valueEncoder #-}

instance ValueEncoder Chronos.Date where
  valueEncoder =
    ModifiedJulianDay . fromIntegral . Chronos.getDay . Chronos.dateToDay >$< date
  {-# INLINE valueEncoder #-}

instance ValueEncoder Chronos.Time where
  valueEncoder =
    Chronos.getTime >$< int8
