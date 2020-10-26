module Polysemy.Hasql.ColumnOptions where

import Polysemy.Db.Data.ColumnOptions (ColumnOptions(ColumnOptions))

format :: ColumnOptions -> Text
format (ColumnOptions unique notNull primaryKey) =
  if primaryKey then " primary key"
  else u <> nn
  where
    u =
      if unique then " unique" else ""
    nn =
      if notNull then " not null" else ""
