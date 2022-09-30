module Polysemy.Hasql.ColumnOptions where

import Polysemy.Db.Data.ColumnOptions (ColumnOptions (ColumnOptions))

import Polysemy.Hasql.Data.SqlCode (SqlCode, esql)

format :: ColumnOptions -> SqlCode
format (ColumnOptions unique notNull primaryKey) =
  if primaryKey then "primary key"
  else [esql|#{u} #{nn}|]
  where
    u =
      if unique then "unique" else ""
    nn =
      if notNull then "not null" else ""
