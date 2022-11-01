module Polysemy.Hasql.Test.WithInitTest where

import Hasql.Decoders (column, int8, nonNullable)
import Polysemy.Db.Atomic (interpretAtomic)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Test (Hedgehog, UnitTest, assertEq)
import Sqel.Statement (unprepared)

import Polysemy.Hasql.Data.InitDb (InitDb (InitDb))
import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (Database)
import Polysemy.Hasql.Test.Run (integrationTest)

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
    stmt = runIdentity <$> unprepared "select 1" (column (nonNullable int8)) mempty
    initDb = (InitDb "test" False \ _ -> atomicModify' (1 +))

test_withInit :: UnitTest
test_withInit = do
  integrationTest do
    assertEq 2 =<< restop @DbError @Database (interpretAtomic 0 prog)
