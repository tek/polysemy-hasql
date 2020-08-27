module Polysemy.Db.Test.Database where

import Control.Lens (Lens', set, view)
import qualified Data.UUID as UUID
import Hasql.Connection (Connection)
import Hasql.Session (QueryError)
import Polysemy.Db.Random (Random, random, runRandomIO)
import Polysemy.Resource (Resource, bracket)

import qualified Polysemy.Db.Data.DbConfig as DbConfig
import Polysemy.Db.Data.DbConfig (DbConfig(DbConfig))
import qualified Polysemy.Db.Data.DbConnection as DbConnection
import Polysemy.Db.Data.DbConnection (DbConnection)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.DbName (DbName(DbName))
import qualified Polysemy.Db.Data.QueryTable as QueryTable
import Polysemy.Db.Data.QueryTable (QueryTable)
import qualified Polysemy.Db.Data.Table as Table
import Polysemy.Db.Data.Table (Table, tableName)
import Polysemy.Db.Data.TableName (TableName(TableName))
import Polysemy.Db.DbConnection (interpretDbConnection)
import Polysemy.Db.Session (convertQueryError)
import qualified Polysemy.Db.Statement as Statement
import Polysemy.Db.Table (createTable, dropTable, runStatement)
import Polysemy.Db.Table.QueryTable (GenQueryTable, genQueryTable)
import Polysemy.Db.Table.Table (GenTable, genTable)

suffixedTable ::
  Lens' t TableName ->
  Text ->
  t ->
  t
suffixedTable lens suffix table =
  set lens suffixedName table
  where
    suffixedName =
      TableName [i|#{canonicalName}-#{suffix}|]
    TableName canonicalName =
      view lens table

bracketTestTable ::
  Members [Resource, Embed IO, DbConnection, Random, Error QueryError, Error DbError] r =>
  Lens' t (Table d) ->
  t ->
  (t -> Sem r a) ->
  Connection ->
  Sem r a
bracketTestTable lens t use connection =
  bracket acquire release (const (use t))
  where
    acquire =
      createTable connection (view Table.structure table)
    release _ =
      dropTable connection (view tableName table)
    table =
      view lens t

withTestTable ::
  Members [Resource, Embed IO, DbConnection, Random, Error QueryError, Error DbError] r =>
  Lens' t (Table d) ->
  t ->
  (t -> Sem r a) ->
  Sem r a
withTestTable lens table use = do
  connection <- fromEither =<< DbConnection.connect
  suffix <- UUID.toText <$> random
  bracketTestTable lens (suffixedTable (lens . tableName) suffix table) use connection

withTestPlainTable ::
  Members [Resource, Embed IO, DbConnection, Random, Error QueryError, Error DbError] r =>
  Table d ->
  (Table d -> Sem r a) ->
  Sem r a
withTestPlainTable =
  withTestTable id

withTestTableGen ::
  ∀ d rep a r .
  Members [Resource, Embed IO, DbConnection, Random, Error QueryError, Error DbError] r =>
  GenTable d rep =>
  (Table d -> Sem r a) ->
  Sem r a
withTestTableGen =
  withTestPlainTable (genTable @_ @rep)

withTestQueryTable ::
  Members [Resource, Embed IO, DbConnection, Random, Error QueryError, Error DbError] r =>
  QueryTable d q ->
  (QueryTable d q -> Sem r a) ->
  Sem r a
withTestQueryTable =
  withTestTable QueryTable.table

withTestQueryTableGen ::
  ∀ d rep q a r .
  Members [Resource, Embed IO, DbConnection, Random, Error QueryError, Error DbError] r =>
  GenQueryTable d rep q =>
  (QueryTable d q -> Sem r a) ->
  Sem r a
withTestQueryTableGen =
  withTestQueryTable (genQueryTable @_ @rep)

createTestDb ::
  Members [Random, DbConnection, Error DbError, Embed IO] r =>
  DbConfig ->
  Connection ->
  Sem r DbConfig
createTestDb dbConfig@(DbConfig _ _ (DbName name) _ _) connection = do
  suffix <- UUID.toText <$> random
  let
    suffixedName = [i|#{name}-#{suffix}|]
    suffixed = dbConfig & DbConfig.name .~ suffixedName
  mapError convertQueryError (runStatement connection () (Statement.createDb suffixedName))
  pure suffixed

withTestDb ::
  Members [Error DbError, Embed IO, Resource] r =>
  DbConfig ->
  (DbConfig -> Sem r a) ->
  Sem r a
withTestDb baseConfig f =
  interpretDbConnection baseConfig do
    connection <- fromEither =<< DbConnection.connect
    bracket (acquire baseConfig connection) (release connection) (raise . f)
  where
    acquire config connection =
      runRandomIO $ createTestDb config connection
    release connection (DbConfig _ _ name _ _) =
      mapError convertQueryError (runStatement connection () (Statement.createDb name))

withTestConnection ::
  Members [Error DbError, Embed IO, Resource] r =>
  DbConfig ->
  Sem (DbConnection : r) a ->
  Sem r a
withTestConnection baseConfig ma =
  withTestDb baseConfig \ dbConfig ->
    interpretDbConnection dbConfig ma
