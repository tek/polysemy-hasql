module Polysemy.Hasql (
  module Polysemy.Hasql.Data.QueryTable,
  HasqlConnection,
) where

import Hasql.Connection (Connection)
import Polysemy.Resume (type (!))

import Polysemy.Db.Data.DbConnectionError (DbConnectionError)
import Polysemy.Hasql.Data.DbConnection (DbConnection)
import Polysemy.Hasql.Data.QueryTable (QueryTable)

type HasqlConnection =
  DbConnection Connection ! DbConnectionError
