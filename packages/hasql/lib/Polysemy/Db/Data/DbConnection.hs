module Polysemy.Db.Data.DbConnection where

import Hasql.Connection (Connection)

import Polysemy.Db.Data.DbError (DbError)

data DbConnection m a where
  Connect :: DbConnection m (Either DbError Connection)
  Reset :: DbConnection m ()

makeSem ''DbConnection
