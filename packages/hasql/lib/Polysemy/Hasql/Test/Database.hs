module Polysemy.Hasql.Test.Database where

import Control.Lens (Lens', view, (%~), (.~))
import qualified Data.UUID as UUID
import Exon (exon)
import Hasql.Connection (Connection)
import Hasql.Session (QueryError)
import qualified Polysemy.Db.Data.DbConfig as DbConfig
import Polysemy.Db.Data.DbConfig (DbConfig (DbConfig))
import Polysemy.Db.Data.DbConnectionError (DbConnectionError)
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.DbName (DbName (DbName))
import Polysemy.Db.Data.Rep (Auto, PrimQuery, PrimaryKey, UidRep)
import Polysemy.Db.Data.Uid (Uid)
import Polysemy.Db.Random (Random, random, runRandomIO)
import Polysemy.Time (GhcTime)

import Polysemy.Hasql.Data.Database (Database)
import qualified Polysemy.Hasql.Data.DbConnection as DbConnection
import Polysemy.Hasql.Data.DbConnection (DbConnection)
import Polysemy.Hasql.Data.DbType (Column (Column), Name (Name), Selector (Selector))
import qualified Polysemy.Hasql.Data.QueryTable as QueryTable
import Polysemy.Hasql.Data.QueryTable (QueryTable, UidQueryTable)
import qualified Polysemy.Hasql.Data.Table as Table
import Polysemy.Hasql.Data.Table (Table)
import Polysemy.Hasql.Database (HasqlConnection, interpretDatabase)
import Polysemy.Hasql.DbConnection (interpretDbConnection)
import Polysemy.Hasql.Session (convertQueryError)
import qualified Polysemy.Hasql.Statement as Statement
import Polysemy.Hasql.Store (StoreStack, interpretStoreDbFull)
import Polysemy.Hasql.Table (createTable, dropTable, runStatement)
import Polysemy.Hasql.Table.BasicSchema (BasicSchema, basicSchema)
import Polysemy.Hasql.Table.Query.Text (sqlQuote)
import Polysemy.Hasql.Table.Query.Update (BuildPartialSql)
import Polysemy.Hasql.Table.Schema (Schema, UidQuerySchema, UidSchema, schema)

suffixedTable ::
  Lens' t (Table d) ->
  Text ->
  t ->
  t
suffixedTable lens suffix =
  lens . Table.structure %~ applySuffix
  where
    applySuffix (Column (Name name) _ tpe opt dbTpe) =
      Column (Name suffixed) (Selector (sqlQuote suffixed)) tpe opt dbTpe
      where
        suffixed =
          [exon|#{name}-#{suffix}|]

bracketTestTable ::
  Members [Resource, Embed IO, DbConnection Connection !! DbConnectionError, Random, Stop QueryError, Stop DbError] r =>
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
      dropTable connection (view Table.name table)
    table =
      view lens t

withTestTable ::
  Members [Resource, Embed IO, DbConnection Connection !! DbConnectionError, Random, Stop QueryError, Stop DbError] r =>
  Lens' t (Table d) ->
  t ->
  (t -> Sem r a) ->
  Sem r a
withTestTable lens table use = do
  suffix <- UUID.toText <$> random
  resumeHoist DbError.Connection $ DbConnection.use \ _ ->
    bracketTestTable lens (suffixedTable lens suffix table) (raise . use)

withTestPlainTable ::
  Members [Resource, Embed IO, DbConnection Connection !! DbConnectionError, Random, Stop QueryError, Stop DbError] r =>
  Table d ->
  (Table d -> Sem r a) ->
  Sem r a
withTestPlainTable =
  withTestTable id

withTestTableGen ::
  ∀ rep d a r .
  Members [Resource, Embed IO, DbConnection Connection !! DbConnectionError, Random, Stop QueryError, Stop DbError] r =>
  BasicSchema rep d =>
  (Table d -> Sem r a) ->
  Sem r a
withTestTableGen =
  withTestPlainTable (basicSchema @rep)

withTestQueryTable ::
  Members [Resource, Embed IO, DbConnection Connection !! DbConnectionError, Random, Stop QueryError, Stop DbError] r =>
  QueryTable q d ->
  (QueryTable q d -> Sem r a) ->
  Sem r a
withTestQueryTable =
  withTestTable QueryTable.table

withTestQueryTableGen ::
  ∀ qrep rep q d a r .
  Members [Resource, Embed IO, DbConnection Connection !! DbConnectionError, Random, Stop QueryError, Stop DbError] r =>
  Schema qrep rep q d =>
  (QueryTable q d -> Sem r a) ->
  Sem r a
withTestQueryTableGen =
  withTestQueryTable (schema @qrep @rep)

createTestDb ::
  Members [Random, DbConnection Connection !! DbConnectionError, Stop DbError, Embed IO] r =>
  DbConfig ->
  Connection ->
  Sem r DbConfig
createTestDb dbConfig@(DbConfig _ _ (DbName name) _ _) connection = do
  suffix <- UUID.toText <$> random
  let
    suffixedName = DbName [exon|#{name}-#{suffix}|]
    suffixed = dbConfig & DbConfig.name .~ suffixedName
  mapStop convertQueryError (runStatement connection () (Statement.createDb suffixedName))
  pure suffixed

-- TODO this should use `Error`
withTestDb ::
  Members [Stop DbError, Resource, Embed IO, Final IO] r =>
  DbConfig ->
  (DbConfig -> Sem r a) ->
  Sem r a
withTestDb baseConfig f =
  interpretDbConnection "test" baseConfig do
    resumeHoist DbError.Connection $ DbConnection.use \ _ connection ->
      bracket (acquire baseConfig connection) (release connection) (raise . raise . f)
  where
    acquire config connection =
      runRandomIO $ createTestDb config connection
    release connection (DbConfig _ _ name _ _) =
      mapStop convertQueryError (runStatement connection () (Statement.dropDb name))

withTestConnection ::
  Members [Stop DbError, Time t dt, Resource, Embed IO, Final IO] r =>
  DbConfig ->
  Sem (Database !! DbError : HasqlConnection : r) a ->
  Sem r a
withTestConnection baseConfig ma =
  withTestDb baseConfig \ dbConfig ->
    interpretDbConnection "test" dbConfig (interpretDatabase ma)

type TestStoreDeps =
  [
    Resource,
    Embed IO,
    HasqlConnection,
    Database !! DbError,
    Error DbError,
    Random,
    Log,
    Stop QueryError,
    Stop DbError,
    GhcTime
  ]

withTestStoreTable ::
  ∀ qrep rep d i r tree a u .
  BuildPartialSql d tree u =>
  Members TestStoreDeps r =>
  Schema qrep rep i (Uid i d) =>
  (UidQueryTable i d -> Sem (StoreStack i d ++ r) a) ->
  Sem r a
withTestStoreTable prog =
  withTestQueryTableGen @qrep @rep \ table ->
    interpretStoreDbFull table (prog table)

withTestStoreTableGenAs ::
  ∀ qrep rep irep d i r tree a u .
  BuildPartialSql d tree u =>
  Members TestStoreDeps r =>
  UidQuerySchema qrep irep rep i i d =>
  (UidQueryTable i d -> Sem (StoreStack i d ++ r) a) ->
  Sem r a
withTestStoreTableGenAs prog =
  withTestQueryTableGen @qrep @(UidRep irep rep) \ table ->
    interpretStoreDbFull table (prog table)

withTestStoreTableGen ::
  ∀ rep i d r tree a u .
  BuildPartialSql d tree u =>
  Members TestStoreDeps r =>
  UidSchema rep i d =>
  (UidQueryTable i d -> Sem (StoreStack i d ++ r) a) ->
  Sem r a
withTestStoreTableGen prog =
  withTestQueryTableGen @(PrimQuery "id") @(UidRep PrimaryKey rep) \ table ->
    interpretStoreDbFull table (prog table)

withTestStore ::
  ∀ qrep rep i d r tree u .
  BuildPartialSql d tree u =>
  Members TestStoreDeps r =>
  Schema qrep rep i (Uid i d) =>
  InterpretersFor (StoreStack i d) r
withTestStore prog =
  withTestStoreTable @qrep @rep (const prog)

withTestStoreGenAs ::
  ∀ qrep irep rep i d r tree u .
  BuildPartialSql d tree u =>
  Members TestStoreDeps r =>
  UidQuerySchema qrep irep rep i i d =>
  InterpretersFor (StoreStack i d) r
withTestStoreGenAs prog =
  withTestStoreTableGenAs @qrep @rep @irep (const prog)

withTestStoreGen ::
  ∀ rep i d r tree u .
  BuildPartialSql d tree u =>
  Members TestStoreDeps r =>
  UidSchema rep i d =>
  InterpretersFor (StoreStack i d) r
withTestStoreGen =
  withTestStoreGenAs @(PrimQuery "id") @PrimaryKey @rep

withTestStoreAuto ::
  ∀ i d r tree a u .
  BuildPartialSql d tree u =>
  Members TestStoreDeps r =>
  UidSchema Auto i d =>
  Sem (StoreStack i d ++ r) a ->
  Sem r a
withTestStoreAuto =
  withTestStoreGen @Auto

withTestStoreUidAs ::
  ∀ qrep irep i d r tree a u .
  Members TestStoreDeps r =>
  BuildPartialSql d tree u =>
  Schema qrep (UidRep irep Auto) i (Uid i d) =>
  Sem (StoreStack i d ++ r) a ->
  Sem r a
withTestStoreUidAs prog =
  withTestQueryTableGen @qrep @(UidRep irep Auto) \ table ->
    interpretStoreDbFull table prog

withTestStoreUid ::
  ∀ i d r tree a u .
  Members TestStoreDeps r =>
  BuildPartialSql d tree u =>
  Schema (PrimQuery "id") (UidRep PrimaryKey Auto) i (Uid i d) =>
  Sem (StoreStack i d ++ r) a ->
  Sem r a
withTestStoreUid prog =
  withTestQueryTableGen @(PrimQuery "id") @(UidRep PrimaryKey Auto) \ table ->
    interpretStoreDbFull table prog
