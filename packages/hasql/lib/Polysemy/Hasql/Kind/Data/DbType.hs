module Polysemy.Hasql.Kind.Data.DbType where

import Polysemy.Db.Data.FieldId (FieldId)

data DbType =
  Prim {
    tpe :: *
  }
  |
  Prod {
    tpe :: *,
    columns :: [Column]
  }
  |
  Sum {
    tpe :: *,
    columns :: [Column]
  }

type family ColumnDataType (dt :: DbType) :: * where
  ColumnDataType ('Prim d) = d
  ColumnDataType ('Prod d _) = d
  ColumnDataType ('Sum d _) = d

data Column =
  Column {
    name :: FieldId,
    eff :: [*],
    dbType :: DbType
  }
