module Polysemy.Hasql.Table.Query.Text where

import qualified Database.PostgreSQL.LibPQ as LibPQ
import qualified Exon
import Hasql.Connection (withLibPQConnection)
import qualified Polysemy.Db.Data.DbConnectionError as DbConnectionError
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.DbName (DbName (unDbName))
import Polysemy.Db.Text.Quote (dquote)

import qualified Polysemy.Hasql.Data.Database as Database
import Polysemy.Hasql.Data.Database (Database)
import qualified Polysemy.Hasql.Data.DbType as Column
import Polysemy.Hasql.Data.DbType (Column)
import Polysemy.Hasql.Data.SqlCode (SqlCode (SqlCode))

commaSeparated ::
  Foldable t =>
  t SqlCode ->
  SqlCode
commaSeparated =
  Exon.intercalate ", "

commaColumns ::
  Functor t =>
  Foldable t =>
  t Column ->
  SqlCode
commaColumns columns =
  commaSeparated (coerce . Column._selector <$> columns)

escape ::
  Members [Database !! DbError, Stop DbError, Embed IO] r =>
  ByteString ->
  Sem r (Maybe ByteString)
escape payload = do
  restop @_ @Database do
    Database.connect (traverseLeft (stop . connError) <=< run)
  where
    run connection =
      tryAny (withLibPQConnection connection (flip LibPQ.escapeStringConn payload))
    connError =
      DbError.Connection . DbConnectionError.Query

sqlQuote :: Text -> SqlCode
sqlQuote =
  SqlCode . dquote

quoteName :: DbName -> SqlCode
quoteName =
  sqlQuote . unDbName
