module Polysemy.Hasql.Test.Database where

import qualified Data.UUID as UUID
import Data.UUID (UUID)
import Exon (exon)
import Hasql.Connection (Connection)
import Hasql.Session (QueryError)
import Polysemy.Db.Data.DbConfig (DbConfig (DbConfig))
import Polysemy.Db.Data.DbConnectionError (DbConnectionError)
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.DbName (DbName (DbName))
import Polysemy.Db.Effect.Random (Random, random)
import Polysemy.Db.Interpreter.Random (interpretRandom)
import Time (GhcTime)
import Sqel.Data.PgTypeName (pattern PgTypeName, pgTableName)
import Sqel (TableSchema)

import Polysemy.Hasql.Effect.Database (ConnectionSource, Database)
import qualified Polysemy.Hasql.Effect.DbConnectionPool as DbConnectionPool
import Polysemy.Hasql.Effect.DbConnectionPool (DbConnectionPool)
import Polysemy.Hasql.Interpreter.Database (interpretDatabase)
import Polysemy.Hasql.Interpreter.DbConnectionPool (interpretDbConnectionPool, interpretDbConnectionPoolSingle)
import Polysemy.Hasql.Session (convertQueryError, runStatement)
import qualified Polysemy.Hasql.Statement as Statement

suffixedTableSchema ::
  Text ->
  TableSchema d ->
  TableSchema d
suffixedTableSchema suffix =
  #pg . #name %~ \ (PgTypeName name) -> pgTableName [exon|#{name}-#{suffix}|]

createTestDb ::
  Members [Random UUID, Stop DbError, Embed IO] r =>
  DbConfig ->
  Connection ->
  Sem r DbConfig
createTestDb dbConfig@(DbConfig _ _ (DbName name) _ _) connection = do
  suffix <- UUID.toText <$> random
  let
    suffixedName = DbName [exon|#{name}-#{suffix}|]
    suffixed = dbConfig & #name .~ suffixedName
  suffixed <$ runStatement connection () (Statement.createDb suffixedName)

withTestDb ::
  Members [Stop DbError, Resource, Mask, Race, Embed IO, Final IO] r =>
  DbConfig ->
  (DbConfig -> Sem r a) ->
  Sem r a
withTestDb baseConfig f =
  interpretDbConnectionPoolSingle baseConfig do
    resumeHoist DbError.Connection $ DbConnectionPool.acquire "test" >>= \ connection ->
      bracket (acquire baseConfig connection) (release connection) (raise . raise . f)
  where
    acquire config connection =
      interpretRandom $ createTestDb config connection
    release connection (DbConfig _ _ name _ _) =
      mapStop convertQueryError (runStatement connection () (Statement.dropDb name))

type TestConnectionEffects =
  [
    Database !! DbError,
    Scoped ConnectionSource (Database !! DbError),
    DbConnectionPool !! DbConnectionError
  ]

withTestConnection ::
  Members [Stop DbError, Time t dt, Log, Resource, Mask, Race, Embed IO, Final IO] r =>
  DbConfig ->
  InterpretersFor TestConnectionEffects r
withTestConnection baseConfig ma =
  withTestDb baseConfig \ dbConfig ->
    interpretDbConnectionPool dbConfig Nothing Nothing $
    interpretDatabase $
    ma

type TestStoreDeps =
  [
    Resource,
    Embed IO,
    Scoped ConnectionSource (Database !! DbError),
    Database !! DbError,
    Error DbError,
    Random UUID,
    Log,
    Stop QueryError,
    Stop DbError,
    GhcTime
  ]
