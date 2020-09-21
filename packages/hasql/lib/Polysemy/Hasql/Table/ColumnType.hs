{-# LANGUAGE UndecidableSuperClasses #-}

module Polysemy.Hasql.Table.ColumnType where

import qualified Chronos as Chronos
import Generics.SOP.GGP (GCode)
import Path (Path)

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

instance ColumnType Chronos.Date where
  columnType =
    "date"

instance ColumnType Chronos.Time where
  columnType =
    "bigint"

instance ColumnType a => ColumnType [a] where
  columnType =
    columnType @a <> "[]"

instance ColumnType a => ColumnType (NonEmpty a) where
  columnType =
    columnType @a <> "[]"

instance ColumnType (Path b t) where
  columnType =
    "text"
