module Polysemy.Hasql.Test.Database where

import Control.Lens (Lens', set, view)
import qualified Data.UUID as UUID
import Hasql.Connection (Connection)
import Hasql.Session (QueryError)
import Polysemy.Db.Random (Random, random, runRandomIO)
import Polysemy.Resource (Resource, bracket)
import Polysemy.Resume (Stop, mapStop, resumeHoist, type (!))
import Polysemy.Time (Time)

import Polysemy.Db.Data.Column (UidRep)
import qualified Polysemy.Db.Data.DbConfig as DbConfig
import Polysemy.Db.Data.DbConfig (DbConfig(DbConfig))
import Polysemy.Db.Data.DbConnectionError (DbConnectionError)
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.DbName (DbName(DbName))
import Polysemy.Db.Data.IdQuery (IdQuery)
import Polysemy.Db.Data.TableName (TableName(TableName))
import Polysemy.Db.Data.Uid (Uid)
import Polysemy.Hasql (HasqlConnection)
import Polysemy.Hasql.Data.Database (Database)
import qualified Polysemy.Hasql.Data.DbConnection as DbConnection
import Polysemy.Hasql.Data.DbConnection (DbConnection)
import qualified Polysemy.Hasql.Data.QueryTable as QueryTable
import Polysemy.Hasql.Data.QueryTable (QueryTable)
import qualified Polysemy.Hasql.Data.Table as Table
import Polysemy.Hasql.Data.Table (Table, tableName)
import Polysemy.Hasql.Database (interpretDatabase)
import Polysemy.Hasql.DbConnection (interpretDbConnection)
import Polysemy.Hasql.Session (convertQueryError)
import qualified Polysemy.Hasql.Statement as Statement
import Polysemy.Hasql.Store (StoreStack, UidStoreStack, interpretStoreDbFull, interpretStoreDbFullUid)
import Polysemy.Hasql.Table (createTable, dropTable, runStatement)
import Polysemy.Hasql.Table.QueryTable (GenQueryTable, genQueryTable)
import Polysemy.Hasql.Table.Representation (Rep)
import Polysemy.Hasql.Table.Table (GenTable, genTable)
import Polysemy.Time (GhcTime)

suffixedTable ::
  Lens' t TableName ->
  Text ->
  t ->
  t
suffixedTable lens suffix table =
  set lens suffixedName table
  where
    suffixedName =
      TableName [qt|#{canonicalName}-#{suffix}|]
    TableName canonicalName =
      view lens table

bracketTestTable ::
  Members [Resource, Embed IO, DbConnection Connection ! DbConnectionError, Random, Stop QueryError, Stop DbError] r =>
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
  Members [Resource, Embed IO, DbConnection Connection ! DbConnectionError, Random, Stop QueryError, Stop DbError] r =>
  Lens' t (Table d) ->
  t ->
  (t -> Sem r a) ->
  Sem r a
withTestTable lens table use = do
  connection <- resumeHoist DbError.Connection DbConnection.connect
  suffix <- UUID.toText <$> random
  bracketTestTable lens (suffixedTable (lens . tableName) suffix table) use connection

withTestPlainTable ::
  Members [Resource, Embed IO, DbConnection Connection ! DbConnectionError, Random, Stop QueryError, Stop DbError] r =>
  Table d ->
  (Table d -> Sem r a) ->
  Sem r a
withTestPlainTable =
  withTestTable id

withTestTableGen ::
  ∀ rep d a r .
  Members [Resource, Embed IO, DbConnection Connection ! DbConnectionError, Random, Stop QueryError, Stop DbError] r =>
  GenTable rep d =>
  (Table d -> Sem r a) ->
  Sem r a
withTestTableGen =
  withTestPlainTable (genTable @rep)

withTestQueryTable ::
  Members [Resource, Embed IO, DbConnection Connection ! DbConnectionError, Random, Stop QueryError, Stop DbError] r =>
  QueryTable q d ->
  (QueryTable q d -> Sem r a) ->
  Sem r a
withTestQueryTable =
  withTestTable QueryTable.table

withTestQueryTableGen ::
  ∀ rep q d a r .
  Members [Resource, Embed IO, DbConnection Connection ! DbConnectionError, Random, Stop QueryError, Stop DbError] r =>
  GenQueryTable rep q d =>
  (QueryTable q d -> Sem r a) ->
  Sem r a
withTestQueryTableGen =
  withTestQueryTable (genQueryTable @rep)

createTestDb ::
  Members [Random, DbConnection Connection ! DbConnectionError, Stop DbError, Embed IO] r =>
  DbConfig ->
  Connection ->
  Sem r DbConfig
createTestDb dbConfig@(DbConfig _ _ (DbName name) _ _) connection = do
  suffix <- UUID.toText <$> random
  let
    suffixedName = [qt|#{name}-#{suffix}|]
    suffixed = dbConfig & DbConfig.name .~ suffixedName
  mapStop convertQueryError (runStatement connection () (Statement.createDb suffixedName))
  pure suffixed

withTestDb ::
  Members [Stop DbError, Embed IO, Resource] r =>
  DbConfig ->
  (DbConfig -> Sem r a) ->
  Sem r a
withTestDb baseConfig f =
  interpretDbConnection baseConfig do
    connection <- resumeHoist DbError.Connection DbConnection.connect
    bracket (acquire baseConfig connection) (release connection) (raise . f)
  where
    acquire config connection =
      runRandomIO $ createTestDb config connection
    release connection (DbConfig _ _ name _ _) =
      mapStop convertQueryError (runStatement connection () (Statement.dropDb name))

withTestConnection ::
  Members [Stop DbError, Embed IO, Time t dt, Resource] r =>
  DbConfig ->
  Sem (Database ! DbError : HasqlConnection : r) a ->
  Sem r a
withTestConnection baseConfig ma =
  withTestDb baseConfig \ dbConfig ->
    interpretDbConnection dbConfig (interpretDatabase ma)

type TestStoreDeps =
  [
    Resource,
    Embed IO,
    HasqlConnection,
    Database ! DbError,
    Error DbError,
    Random,
    Stop QueryError,
    Stop DbError,
    GhcTime
  ]

withTestStoreTableGen ::
  ∀ rep q d r a .
  Members TestStoreDeps r =>
  GenQueryTable rep q d =>
  (QueryTable q d -> Sem (StoreStack q d q d ++ r) a) ->
  Sem r a
withTestStoreTableGen prog =
  withTestQueryTableGen @rep \ table ->
    interpretStoreDbFull table (prog table)

withTestStoreTableUidGen ::
  ∀ rep ir d i r a .
  Members TestStoreDeps r =>
  GenQueryTable (UidRep ir rep) (IdQuery i) (Uid i d) =>
  (QueryTable (IdQuery i) (Uid i d) -> Sem (UidStoreStack i d ++ r) a) ->
  Sem r a
withTestStoreTableUidGen prog =
  withTestQueryTableGen @(UidRep ir rep) \ table ->
    interpretStoreDbFullUid table (prog table)

withTestStoreGen ::
  ∀ rep q d r .
  Members TestStoreDeps r =>
  GenQueryTable rep q d =>
  InterpretersFor (StoreStack q d q d) r
withTestStoreGen prog =
  withTestQueryTableGen @rep \ table ->
    interpretStoreDbFull table prog

withTestStore ::
  ∀ q d r a .
  Members TestStoreDeps r =>
  GenQueryTable (Rep d) q d =>
  Sem (StoreStack q d q d ++ r) a ->
  Sem r a
withTestStore prog =
  withTestQueryTableGen @(Rep d) \ table ->
    interpretStoreDbFull table prog

withTestStoreUid ::
  ∀ i d r a .
  Members TestStoreDeps r =>
  GenQueryTable (Rep (Uid i d)) (IdQuery i) (Uid i d) =>
  Sem (UidStoreStack i d ++ r) a ->
  Sem r a
withTestStoreUid prog =
  withTestQueryTableGen @(Rep (Uid i d)) \ table ->
    interpretStoreDbFullUid table prog
