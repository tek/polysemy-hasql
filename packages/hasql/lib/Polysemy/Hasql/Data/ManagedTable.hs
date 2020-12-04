module Polysemy.Hasql.Data.ManagedTable where

import Hasql.Statement (Statement)

import Polysemy.Time (TimeUnit)

data ManagedTable d :: Effect where
  RunStatement :: q -> Statement q o -> ManagedTable d m o
  RetryStatement :: TimeUnit t => t -> q -> Statement q o -> ManagedTable d m o

makeSem ''ManagedTable
