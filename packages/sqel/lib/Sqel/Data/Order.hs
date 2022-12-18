module Sqel.Data.Order where

import Sqel.Data.Sql (ToSql (toSql), sql)

data Order =
  Asc
  |
  Desc
  |
  Using Text
  deriving stock (Eq, Show, Generic)

instance ToSql Order where
  toSql = \case
    Asc -> "asc"
    Desc -> "desc"
    Using expr -> [sql|using ##{expr}|]
