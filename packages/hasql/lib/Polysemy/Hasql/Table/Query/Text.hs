module Polysemy.Hasql.Table.Query.Text where

import qualified Database.PostgreSQL.LibPQ as LibPQ
import Hasql.Connection (withLibPQConnection)
import qualified Polysemy.Db.Data.DbConnectionError as DbConnectionError
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.DbName (DbName (unDbName))
import Sqel.Data.Sql (Sql, sqlQuote)

import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (Database)

escape ::
  Members [Database !! DbError, Stop DbError, Embed IO] r =>
  ByteString ->
  Sem r (Maybe ByteString)
escape payload = do
  restop @_ @Database do
    Database.use (either (stop . connError) pure <=< run)
  where
    run connection =
      tryAny (withLibPQConnection connection (flip LibPQ.escapeStringConn payload))
    connError =
      DbError.Connection . DbConnectionError.Query

quoteName :: DbName -> Sql
quoteName =
  sqlQuote . unDbName
