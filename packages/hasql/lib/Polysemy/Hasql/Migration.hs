module Polysemy.Hasql.Migration where

import Generics.SOP (SListI)
import Polysemy.Db.Data.DbError (DbError)
import Sqel.Data.Migration (Migrations, Migs, MkMigrations, hoistMigrations, migrate)
import Sqel.Migration.Statement (migrationStatements)
import Sqel.Migration.Table (AutoMigrationEffect (autoMigrationEffect))

import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (Database)

newtype MigrateSem r a =
  MigrateSem { unMigrateSem :: Sem r a }
  deriving stock (Generic)

instance (
    Members [Database !! DbError, Stop DbError] r
  ) => AutoMigrationEffect (MigrateSem r) where
    autoMigrationEffect _ _ actions =
      MigrateSem (restop (Database.session (migrationStatements actions)))

migrateSem ::
  SListI (Migs old cur) =>
  MkMigrations arg (MigrateSem r) (Migs old cur) =>
  arg ->
  Migrations (Sem r) old cur
migrateSem =
  hoistMigrations unMigrateSem . migrate
