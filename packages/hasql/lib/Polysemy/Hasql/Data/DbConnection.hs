module Polysemy.Hasql.Data.DbConnection where

import Polysemy.Db.Data.DbError (DbError)

data DbConnection c :: Effect where
  Connect :: DbConnection c m (Either DbError c)
  Disconnect :: DbConnection c m (Either DbError ())
  Reset :: DbConnection c m ()

makeSem ''DbConnection
