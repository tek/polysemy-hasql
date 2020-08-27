module Polysemy.Db.ColumnParams where

import Polysemy.Db.Data.ColumnParams (ColumnParams(ColumnParams))

format :: ColumnParams -> Text
format (ColumnParams unique notNull primaryKey) =
  if primaryKey then " primary key"
  else u <> nn
  where
    u =
      if unique then " unique" else ""
    nn =
      if notNull then " not null" else ""
