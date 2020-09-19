{-# LANGUAGE UndecidableSuperClasses #-}

module Polysemy.Hasql.Table.ColumnType where

import qualified Chronos as Chronos
import Data.Vector (Vector)
import Generics.SOP (Code, Generic)
import Path (Path)
import Prelude hiding (Enum, Generic)

import Polysemy.Db.Data.Column (Auto, Enum, Flatten, Prim, Sum)

type family SumRep rep :: * where
  SumRep Auto = Auto
  SumRep (Sum rep) = rep
  SumRep rep = rep

data PrimColumn (r :: *) (d :: *) =
  PrimColumn
  deriving (Show)

data EnumColumn (r :: *) (d :: *) (dss :: [[*]]) =
  EnumColumn
  deriving (Show)

data SumColumn r (d :: *) (dss :: [[*]]) =
  SumColumn
  deriving (Show)

type family PrimColumnCode r d :: * where
  PrimColumnCode (Prim r) d = PrimColumn r d
  PrimColumnCode r d = PrimColumn r d

type family DataColumnCode r (d :: *) (dss :: [[*]]) (initial :: [[*]]) :: * where
  DataColumnCode (Enum r) d _ initial = EnumColumn r d initial
  DataColumnCode (Sum r) d ((d1 : ds) : dss) initial = SumColumn r d initial
  DataColumnCode (Flatten r) d dss initial = PrimColumn (Flatten r) d
  DataColumnCode Auto d ((d1 : ds) : dss) initial = SumColumn Auto d initial
  DataColumnCode r d '[] initial = EnumColumn r d initial
  DataColumnCode r d ('[] : dss) initial = DataColumnCode r d dss initial

type family ColumnCode r (d :: *) :: * where
  ColumnCode (Prim r) d = PrimColumnCode r d
  ColumnCode r Int = PrimColumnCode r Int
  ColumnCode r Text = PrimColumnCode r Text
  ColumnCode r ByteString = PrimColumnCode r ByteString
  ColumnCode r String = PrimColumnCode r String
  ColumnCode r UUID = PrimColumnCode r UUID
  ColumnCode r Float = PrimColumnCode r Float
  ColumnCode r Double = PrimColumnCode r Double
  ColumnCode r [a] = PrimColumnCode r [a]
  ColumnCode r (NonEmpty a) = PrimColumnCode r (NonEmpty a)
  ColumnCode r (Vector a) = PrimColumnCode r (Vector a)
  ColumnCode r d = DataColumnCode r d (Code d) (Code d)

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

instance ColumnType a => ColumnType (PrimColumn a) where
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
