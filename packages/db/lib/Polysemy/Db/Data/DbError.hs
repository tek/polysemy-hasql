module Polysemy.Db.Data.DbError where

import Polysemy.Db.Data.DbConnectionError (DbConnectionError)

data DbError =
  Connection DbConnectionError
  |
  Query Text
  |
  Table Text
  |
  Unexpected Text
  deriving stock (Eq, Show)

instance IsString DbError where
  fromString =
    Table . fromString
