module Polysemy.Hasql.Table.ValueEncoder where

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

instance {-# overlappable #-} (Show a, Generic a, Code a ~ ('[] : cs)) => GenEncoder a ('[] : cs) where
  genEncoder =
    enumEncoder

instance (Coercible c a, Generic a, Code a ~ '[ '[c]], ValueEncoder c) => GenEncoder a '[ '[c]] where
  genEncoder =
    coerce >$< valueEncoder @c

instance {-# overlappable #-} GenEncoder a b => ValueEncoder a where
  valueEncoder =
    genEncoder @a

instance ValueEncoder Bool where
  valueEncoder =
    bool
  {-# inline valueEncoder #-}

instance ValueEncoder Int16 where
  valueEncoder =
    int2
  {-# inline valueEncoder #-}

instance ValueEncoder Int32 where
  valueEncoder =
    int4
  {-# inline valueEncoder #-}

instance ValueEncoder Int64 where
  valueEncoder =
    int8
  {-# inline valueEncoder #-}

instance ValueEncoder Int where
  valueEncoder =
    contramap fromIntegral int8
  {-# inline valueEncoder #-}

instance ValueEncoder Float where
  valueEncoder =
    float4
  {-# inline valueEncoder #-}

instance ValueEncoder Double where
  valueEncoder =
    float8
  {-# inline valueEncoder #-}

instance ValueEncoder Scientific where
  valueEncoder =
    numeric
  {-# inline valueEncoder #-}

instance ValueEncoder Char where
  valueEncoder =
    char
  {-# inline valueEncoder #-}

instance ValueEncoder Text where
  valueEncoder =
    text
  {-# inline valueEncoder #-}

instance ValueEncoder ByteString where
  valueEncoder =
    bytea
  {-# inline valueEncoder #-}

instance ValueEncoder Day where
  valueEncoder =
    date
  {-# inline valueEncoder #-}

instance ValueEncoder LocalTime where
  valueEncoder =
    timestamp
  {-# inline valueEncoder #-}

instance ValueEncoder UTCTime where
  valueEncoder =
    timestamptz
  {-# inline valueEncoder #-}

instance ValueEncoder TimeOfDay where
  valueEncoder =
    time
  {-# inline valueEncoder #-}

instance ValueEncoder (TimeOfDay, TimeZone) where
  valueEncoder =
    timetz
  {-# inline valueEncoder #-}

instance ValueEncoder DiffTime where
  valueEncoder =
    interval
  {-# inline valueEncoder #-}

instance ValueEncoder UUID where
  valueEncoder =
    uuid
  {-# inline valueEncoder #-}

instance ValueEncoder (Path b t) where
  valueEncoder =
    (toText . toFilePath) >$< text
  {-# inline valueEncoder #-}

instance ValueEncoder Chronos.Date where
  valueEncoder =
    ModifiedJulianDay . fromIntegral . Chronos.getDay . Chronos.dateToDay >$< date
  {-# inline valueEncoder #-}

instance ValueEncoder Chronos.Time where
  valueEncoder =
    Chronos.getTime >$< int8
