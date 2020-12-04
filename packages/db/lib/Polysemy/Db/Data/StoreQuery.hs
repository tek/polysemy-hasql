module Polysemy.Db.Data.StoreQuery where

data StoreQuery q e o m a where
  Basic :: q -> StoreQuery q e o m o

makeSem ''StoreQuery
