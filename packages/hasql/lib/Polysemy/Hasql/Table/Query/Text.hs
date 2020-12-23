module Polysemy.Hasql.Table.Query.Text where

import qualified Data.Text as Text
import qualified Database.PostgreSQL.LibPQ as LibPQ
import Hasql.Connection (withLibPQConnection)
import qualified Polysemy.Db.Data.DbConnectionError as DbConnectionError
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)

import qualified Polysemy.Hasql.Data.Database as Database
import Polysemy.Hasql.Data.Database (Database)
import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode, unSqlCode))
import qualified Polysemy.Hasql.Data.DbType as Column
import Polysemy.Hasql.Data.DbType (Column)

commaSeparated ::
  Foldable t =>
  t Text ->
  Text
commaSeparated =
  Text.intercalate ", " . toList

commaColumns ::
  Functor t =>
  Foldable t =>
  t Column ->
  Text
commaColumns columns =
  commaSeparated (coerce . Column._selector <$> columns)

commaSeparatedSql ::
  Functor t =>
  Foldable t =>
  t SqlCode ->
  SqlCode
commaSeparatedSql =
  SqlCode . commaSeparated . fmap unSqlCode

escape ::
  Members [Database !! DbError, Stop DbError, Embed IO] r =>
  ByteString ->
  Sem r (Maybe ByteString)
escape payload = do
  restop do
    Database.connect (traverseLeft (stop . connError) <=< run)
  where
    run connection =
      tryAny (withLibPQConnection connection (flip LibPQ.escapeStringConn payload))
    connError =
      DbError.Connection . DbConnectionError.Query
