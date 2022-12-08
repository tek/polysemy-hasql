module Polysemy.Hasql.Migration where

import Generics.SOP (All)
import qualified Log
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import Sqel.Class.MigrationEffect (MigrationEffect (error, log, runMigrationStatements, runStatement, runStatement_))
import Sqel.Data.Migration (
  CustomMigration,
  HoistMigration (hoistMigration),
  HoistMigrations (hoistMigrations), Migrations,
  )
import Sqel.Migration.Statement (migrationSession)
import qualified Sqel.Migration.Transform as Transform
import Sqel.Migration.Transform (MigrateTransform (MigrateTransform))

import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (Database)

newtype MigrateSem r a =
  MigrateSem { unMigrateSem :: Sem (Database : Stop DbError : r) a }
  deriving stock (Generic, Functor)
  deriving newtype (Applicative, Monad)

type SemMigrations r migs = Migrations (MigrateSem r) migs

type HoistSemMigrations extra r migs migs' =
  HoistMigrations (MigrateSem r) (MigrateSem (extra ++ r)) migs migs'

type CustomSemMigrations r migs =
  All (CustomMigration (MigrateSem r)) migs

instance HoistMigration (MigrateSem r) (MigrateSem r') (MigrateTransform (MigrateSem r) old new) (MigrateTransform (MigrateSem r') old new) where
  hoistMigration f MigrateTransform {..} = MigrateTransform {trans = f . trans, ..}

hoistSemMigrations ::
  ∀ extra r migs migs' .
  HoistSemMigrations extra r migs migs' =>
  (∀ x . Sem (Database : Stop DbError : r) x -> Sem (Database : Stop DbError : extra ++ r) x) ->
  SemMigrations r migs ->
  SemMigrations (extra ++ r) migs'
hoistSemMigrations f m =
  hoistMigrations (MigrateSem . f . unMigrateSem) m

instance (
    Member Log r
  ) => MigrationEffect (MigrateSem r) where
    runMigrationStatements actions =
      MigrateSem (Database.session (migrationSession actions))

    runStatement_ q s = MigrateSem (Database.statement q s)

    runStatement q s = MigrateSem (Database.statement q s)

    log = MigrateSem . Log.debug

    error msg = MigrateSem do
      Log.error msg
      stop (DbError.Table msg)
