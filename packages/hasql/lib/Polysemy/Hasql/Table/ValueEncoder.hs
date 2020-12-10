module Polysemy.Hasql.Table.ValueEncoder where

import qualified Chronos as Chronos
import Data.Scientific (Scientific)
import Data.Time (
  Day(ModifiedJulianDay),
  DiffTime,
  LocalTime(LocalTime),
  TimeOfDay(TimeOfDay),
  TimeZone,
  UTCTime,
  )
import Hasql.Encoders (jsonBytes, 
  Value,
  bool,
  bytea,
  char,
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
import Path (Path, toFilePath)
import Polysemy.Db.Data.Column (Json)
import Prelude hiding (Enum, bool)

import Polysemy.Db.Data.Column (Enum, NewtypePrim, Prim)
import Polysemy.Db.SOP.Constraint (NewtypeCoded)
import qualified Data.Aeson as Aeson

class ValueEncoder a where
  valueEncoder :: Value a

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

chronosToDay :: Chronos.Date -> Day
chronosToDay =
  ModifiedJulianDay . fromIntegral . Chronos.getDay . Chronos.dateToDay

instance ValueEncoder Chronos.Date where
  valueEncoder =
    chronosToDay >$< date
  {-# inline valueEncoder #-}

instance ValueEncoder Chronos.Time where
  valueEncoder =
    Chronos.getTime >$< int8
  {-# inline valueEncoder #-}

chronosToTimeOfDay :: Chronos.TimeOfDay -> TimeOfDay
chronosToTimeOfDay (Chronos.TimeOfDay h m ns) =
  TimeOfDay h m (realToFrac ns / 1000000000)

datetimeToLocalTime :: Chronos.Datetime -> LocalTime
datetimeToLocalTime (Chronos.Datetime d t) =
  LocalTime (chronosToDay d) (chronosToTimeOfDay t)

instance ValueEncoder Chronos.Datetime where
  valueEncoder =
     datetimeToLocalTime >$< valueEncoder
  {-# inline valueEncoder #-}

class RepEncoder (rep :: *) (a :: *) where
  repEncoder :: Value a

instance {-# overlappable #-} ValueEncoder a => RepEncoder r a where
  repEncoder =
    valueEncoder

instance RepEncoder r a => RepEncoder (Prim r) a where
  repEncoder =
    repEncoder @r

instance Show a => RepEncoder (Enum r) a where
  repEncoder =
    enum show

instance ToJSON a => RepEncoder (Json r) a where
  repEncoder =
    (toStrict . Aeson.encode) >$< jsonBytes

instance (NewtypeCoded a c, RepEncoder r c) => RepEncoder (NewtypePrim r) a where
  repEncoder =
    coerce >$< repEncoder @r @c
