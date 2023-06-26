module Polysemy.Hasql.Test.RetryTest where

import qualified Hasql.Connection as Hasql
import Hasql.Decoders (column, int8, nonNullable)
import Polysemy.Db.Data.DbConnectionError (DbConnectionError)
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Test (Hedgehog, UnitTest, assertEq)
import Sqel.Statement (unsafeSql)
import Time (Seconds (Seconds))

import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (Databases, withDatabaseUnique)
import qualified Polysemy.Hasql.Effect.DbConnectionPool as DbConnectionPool
import Polysemy.Hasql.Effect.DbConnectionPool (DbConnectionPool)
import Polysemy.Hasql.Test.RunIntegration (integrationTest)

prog ::
  Members [Hedgehog IO, Databases, DbConnectionPool !! DbConnectionError, Stop DbError, Embed IO] r =>
  Sem r ()
prog = do
  withDatabaseUnique (Just "retry") do
    assertEq 1 =<< restop run
    conn <- stopNote "no connection" =<< resumeHoist DbError.Connection (DbConnectionPool.unsafeGet "retry")
    embed (Hasql.release conn)
    assertEq 1 =<< restop (Database.retry (Seconds 1) (Just 1) run)
  where
    run = Database.statement () stmt
    stmt = runIdentity <$> unsafeSql "select 1" mempty (column (nonNullable int8))

test_retry :: UnitTest
test_retry =
  integrationTest do
    prog
