module Polysemy.Hasql.Test.WithInitTest where

import Conc (interpretAtomic)
import Hasql.Decoders (column, int8, nonNullable)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Test (Hedgehog, UnitTest, assertEq)
import Sqel.Statement (unsafeSql)

import Polysemy.Hasql.Data.InitDb (InitDb (InitDb))
import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (Database)
import Polysemy.Hasql.Test.RunIntegration (integrationTest)

prog ::
  Members [Hedgehog IO, Database, AtomicState Int] r =>
  Sem r Int
prog = do
  assertEq 1 =<< run
  Database.release
  assertEq 1 =<< run
  assertEq 1 =<< run
  pure ()
  atomicGet
  where
    run = Database.withInit initDb (Database.statement () stmt)
    stmt = runIdentity <$> unsafeSql "select 1" mempty (column (nonNullable int8))
    initDb = (InitDb "test" False \ _ -> atomicModify' (1 +))

test_withInit :: UnitTest
test_withInit = do
  integrationTest do
    assertEq 2 =<< restop @DbError @Database (interpretAtomic 0 prog)
