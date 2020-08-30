module Polysemy.Db.Data.StoreQuery where

import Polysemy.Db.Data.StoreError (StoreError)

data StoreQuery q e o m a where
  Basic :: q -> StoreQuery q e o m (Either (StoreError e) o)

makeSem ''StoreQuery

basicQuery ::
  ∀ q o e r .
  Members [Error (StoreError e), StoreQuery q e o] r =>
  q ->
  Sem r o
basicQuery =
  fromEither <=< basic @q @e @o
