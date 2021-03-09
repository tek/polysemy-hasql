module Polysemy.Hasql.Data.ManagedTable where

import Hasql.Statement (Statement)
import Polysemy.Time (TimeUnit)

import Polysemy.Hasql.Data.Table (Table)

data ManagedTable d :: Effect where
  Table :: ManagedTable d m (Table d)
  RunStatement :: q -> Statement q o -> ManagedTable d m o
  RetryStatement :: TimeUnit t => t -> q -> Statement q o -> ManagedTable d m o

makeSem ''ManagedTable
