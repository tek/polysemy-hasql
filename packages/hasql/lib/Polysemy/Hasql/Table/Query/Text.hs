module Polysemy.Hasql.Table.Query.Text where

import qualified Data.Text as Text
import qualified Database.PostgreSQL.LibPQ as LibPQ
import Hasql.Connection (withLibPQConnection)
import qualified Polysemy.Db.Data.DbConnectionError as DbConnectionError
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)

import qualified Polysemy.Db.Data.TableStructure as Column
import Polysemy.Db.Data.TableStructure (Column)
import Polysemy.Db.Text.Quote (dquote)
import qualified Polysemy.Hasql.Data.Database as Database
import Polysemy.Hasql.Data.Database (Database)
import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode, unSqlCode))

commaSeparated ::
  Foldable t =>
  t Text ->
  Text
commaSeparated =
  Text.intercalate ", " . toList

commaFields ::
  Foldable t =>
  t Text ->
  Text
commaFields =
  commaSeparated . fmap dquote . toList

commaColumns ::
  [Column] ->
  Text
commaColumns columns =
  commaFields (Column.columnName <$> columns)

commaSeparatedSql ::
  Functor t =>
  Foldable t =>
  t SqlCode ->
  SqlCode
commaSeparatedSql =
  SqlCode . commaSeparated . fmap unSqlCode

escape ::
  Members [Database ! DbError, Stop DbError, Embed IO] r =>
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
