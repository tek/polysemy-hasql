module Polysemy.Hasql.Data.Database where

import Hasql.Statement (Statement)

import Polysemy.Db.Data.TableStructure (TableStructure)
import Polysemy.Time (TimeUnit)

data Database :: Effect where
  Run :: Maybe TableStructure -> q -> Statement q o -> Database m o
  Retrying :: TimeUnit t => Maybe TableStructure -> t -> q -> Statement q o -> Database m o

makeSem ''Database
