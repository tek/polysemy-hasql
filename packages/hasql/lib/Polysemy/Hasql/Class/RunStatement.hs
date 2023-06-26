module Polysemy.Hasql.Class.RunStatement where

import Polysemy.Db.Data.DbError (DbError)
import Sqel (ResultShape, Statement)

import Polysemy.Hasql.Data.SafeStatement (pattern SafeStatement, safeStatement)
import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (Database)
import qualified Polysemy.Hasql.Effect.DbTable as DbTable
import Polysemy.Hasql.Effect.DbTable (DbTable)

type RunStatement :: [Type] -> [Effect] -> Constraint
class RunStatement tables r where
  runStatement ::
    ResultShape proj result =>
    Bool ->
    query ->
    Statement tables query proj ->
    Sem (Stop DbError : r) result

instance (
    Member (Database !! DbError) r
  ) => RunStatement '[] r where
  runStatement prep q statement =
    restop (Database.statement q hs)
    where
      SafeStatement hs = safeStatement prep statement

instance (
    Member (DbTable table !! DbError) r
  ) => RunStatement '[table] r where
    runStatement prep q statement =
      restop (DbTable.statement prep q statement)

instance (
    Member (DbTable table0 !! DbError) r,
    RunStatement (table1 : tables) r
  ) => RunStatement (table0 : table1 : tables) r where
    runStatement prep q statement =
      restop (DbTable.withTable statement (raise . runStatement prep q))
