module Polysemy.Hasql.Data.Database where

import Hasql.Statement (Statement)

data Database e d m a where
  Run :: q -> Statement q o -> Database e d m (Either e o)

makeSem ''Database
