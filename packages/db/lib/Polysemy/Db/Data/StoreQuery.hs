module Polysemy.Db.Data.StoreQuery where

data StoreQuery q o m a where
  Basic :: q -> StoreQuery q o m o

makeSem ''StoreQuery
