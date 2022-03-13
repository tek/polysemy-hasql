module Polysemy.Db.Data.DbError where

import Polysemy.Db.Data.DbConnectionError (DbConnectionError)

data DbError =
  Connection DbConnectionError
  |
  Query Text
  |
  Table Text
  deriving stock (Eq, Show)
