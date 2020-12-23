module Polysemy.Hasql.Data.ExistingColumn where

import Polysemy.Hasql.Data.DbType (Name)

data ExistingColumn =
  ExistingColumn {
    name :: Name,
    ctype :: Text
  }
  deriving (Eq, Show, Ord)
