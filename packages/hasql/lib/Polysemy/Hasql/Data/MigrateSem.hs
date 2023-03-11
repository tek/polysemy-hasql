{-# language FieldSelectors #-}

module Polysemy.Hasql.Data.MigrateSem where

import qualified Log
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import Sqel.Class.MigrationEffect (MigrationEffect (..))
import Sqel.Ext (HoistMigration (hoistMigration))
import Sqel.Migration.Statement (migrationSession)
import Sqel.Migration.Transform (MigrateTransform (..))

import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (Database)

newtype MigrateSem r a =
  MigrateSem { unMigrateSem :: Sem (Database : Stop DbError : r) a }
  deriving stock (Generic, Functor)
  deriving newtype (Applicative, Monad)

instance HoistMigration (MigrateSem r) (MigrateSem r') (MigrateTransform (MigrateSem r) old new) (MigrateTransform (MigrateSem r') old new) where
  hoistMigration f MigrateTransform {..} = MigrateTransform {trans = f . trans, ..}

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
