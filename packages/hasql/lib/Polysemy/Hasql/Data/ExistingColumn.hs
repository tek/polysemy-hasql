module Polysemy.Hasql.Data.ExistingColumn where

import Sqel.Data.PgType (PgColumnName)

data ExistingColumn =
  ExistingColumn {
    columnName :: PgColumnName,
    dataType :: Text,
    udtName :: Text
  }
  deriving stock (Eq, Show, Ord)
