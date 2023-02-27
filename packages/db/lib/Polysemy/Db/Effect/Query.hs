module Polysemy.Db.Effect.Query where

data Query q o :: Effect where
  Query :: q -> Query q o m o

makeSem ''Query
