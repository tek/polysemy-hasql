module Polysemy.Hasql.Data.ManagedQueryTable where

import Hasql.Statement (Statement)
import Polysemy.Time (TimeUnit)

data ManagedQueryTable q d :: Effect where
  RunStatement :: q -> Statement q o -> ManagedQueryTable q d m o
  RetryStatement :: TimeUnit t => t -> q -> Statement q o -> ManagedQueryTable q d m o

makeSem ''ManagedQueryTable
