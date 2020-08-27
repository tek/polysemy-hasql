module Polysemy.Db.Data.Database where

import Hasql.Statement (Statement)

data Database d e m a where
  Run :: q -> Statement q o -> Database d e m (Either e o)

makeSem ''Database
