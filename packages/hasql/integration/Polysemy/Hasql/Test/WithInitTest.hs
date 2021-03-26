module Polysemy.Hasql.Test.WithInitTest where

import Polysemy.Db.Atomic (interpretAtomic)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Test (UnitTest, assertEq)

import qualified Polysemy.Hasql.Data.Database as Database
import Polysemy.Hasql.Data.Database (Database, InitDb(InitDb))
import qualified Polysemy.Hasql.Data.DbConnection as DbConnection
import Polysemy.Hasql.Database (HasqlConnection)
import Polysemy.Hasql.Test.Run (integrationTest)

prog ::
  Members [Database, HasqlConnection, AtomicState Int] r =>
  Sem r Int
prog = do
  Database.withInit (InitDb "test" \ _ -> atomicModify' (1 +)) do
    () <- Database.sql () "select 1"
    resume_ DbConnection.disconnect
    () <- Database.sql () "select 1"
    pure ()
  atomicGet

test_withInit :: UnitTest
test_withInit = do
  integrationTest do
      assertEq 2 =<< restop @DbError @Database (interpretAtomic 0 prog)
