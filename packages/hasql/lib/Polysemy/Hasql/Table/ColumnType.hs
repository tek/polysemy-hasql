{-# LANGUAGE UndecidableSuperClasses #-}

module Polysemy.Hasql.Table.ColumnType where

import qualified Chronos as Chronos
import Generics.SOP (Code, Generic)
import Path (Path)
import Prelude hiding (Generic)

class ColumnType a where
  columnType :: Text

class (Generic a, b ~ Code a) => GenColumnType a b where
  genColumnType :: Text

instance (Generic a, Code a ~ '[ '[] ]) => GenColumnType a '[ '[] ] where
  genColumnType =
    "text"

instance (Generic a, Code a ~ ('[] : cs)) => GenColumnType a ('[] : cs) where
  genColumnType =
    "text"

instance (Coercible c a, Generic a, Code a ~ '[ '[c]], ColumnType c) => GenColumnType a '[ '[c]] where
  genColumnType =
    columnType @c

instance {-# overlappable #-} (Generic a, GenColumnType a b) => ColumnType a where
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
