module Polysemy.Db.Data.StoreQuery where

import Polysemy.Db.Data.StoreError (StoreError)

data StoreQuery q o e m a where
  Basic :: q -> StoreQuery q o e m (Either (StoreError e) o)

makeSem ''StoreQuery

basicQuery ::
  Members [Error (StoreError e), StoreQuery q o e] r =>
  q ->
  Sem r o
basicQuery =
  fromEither <=< basic
