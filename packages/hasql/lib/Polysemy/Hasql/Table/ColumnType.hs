module Polysemy.Hasql.Table.ColumnType where

import qualified Chronos as Chronos
import Data.Time (Day, DiffTime, LocalTime, TimeOfDay, TimeZone, UTCTime)
import Generics.SOP.GGP (GCode)
import Path (Path)
import Prelude hiding (Enum)

import Polysemy.Db.Data.Column (Enum, Flatten, NewtypePrim, Prim, Sum)

class ColumnType a where
  columnType :: Text

class b ~ GCode a => GenColumnType a b where
  genColumnType :: Text

instance GCode a ~ '[ '[] ] => GenColumnType a '[ '[] ] where
  genColumnType =
    "text"

instance GCode a ~ ('[] : cs) => GenColumnType a ('[] : cs) where
  genColumnType =
    "text"

instance (Coercible c a, GCode a ~ '[ '[c]], ColumnType c) => GenColumnType a '[ '[c]] where
  genColumnType =
    columnType @c

instance {-# overlappable #-} (GenColumnType a b) => ColumnType a where
  columnType =
    genColumnType @a

instance ColumnType a => ColumnType (Maybe a) where
  columnType =
    columnType @a

instance ColumnType Int where
  columnType =
    "bigint"

instance ColumnType Double where
  columnType =
    "double precision"

instance ColumnType Text where
  columnType =
    "text"

instance ColumnType UUID where
  columnType =
    "uuid"

instance ColumnType Day where
  columnType =
    "date"

instance ColumnType LocalTime where
  columnType =
    "timestamp without time zone"

instance ColumnType UTCTime where
  columnType =
    "timestamp with time zone"

instance ColumnType TimeOfDay where
  columnType =
    "time without time zone"

instance ColumnType (TimeOfDay, TimeZone) where
  columnType =
    "time with time zone"

instance ColumnType DiffTime where
  columnType =
    "interval"

instance ColumnType Chronos.Date where
  columnType =
    "date"

instance ColumnType Chronos.Time where
  columnType =
    "bigint"

instance ColumnType Chronos.Datetime where
  columnType =
    "timestamp without time zone"

instance ColumnType a => ColumnType [a] where
  columnType =
    columnType @a <> "[]"

instance ColumnType a => ColumnType (NonEmpty a) where
  columnType =
    columnType @a <> "[]"

instance ColumnType (Path b t) where
  columnType =
    "text"

data Done =
  Done
  deriving (Show)

data Single (rep :: *) (tail :: [*]) =
  Single
  deriving (Show)

data Multi (head :: *) (tail :: [*]) =
  Multi
  deriving (Show)

type family UnconsRep (reps :: [*]) :: *
type instance UnconsRep '[] = Done
type instance UnconsRep (Sum r : reps) = Multi r reps
type instance UnconsRep (Flatten r : reps) = Multi r reps
type instance UnconsRep (Prim r : reps) = Single (Prim r) reps
type instance UnconsRep (NewtypePrim r : reps) = Single (NewtypePrim r) reps
type instance UnconsRep (Enum r : reps) = Single (Enum r) reps
