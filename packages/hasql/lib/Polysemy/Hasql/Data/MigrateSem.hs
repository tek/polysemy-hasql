{-# language FieldSelectors #-}

module Polysemy.Hasql.Data.MigrateSem where

import qualified Log
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import Sqel (unprepared)
import Sqel.Migration (MigrationEffect (..), migrationSession)

import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (Database)

newtype MigrateSem r a =
  MigrateSem { unMigrateSem :: Sem (Database : Stop DbError : r) a }
  deriving stock (Generic, Functor)
  deriving newtype (Applicative, Monad)

instance (
    Member Log r
  ) => MigrationEffect (MigrateSem r) where
    runMigrationStatements actions =
      MigrateSem (Database.session (migrationSession actions))

    runStatement_ q s = MigrateSem (Database.statement q (unprepared s))

    runStatement q s = MigrateSem (Database.statement q (unprepared s))

    log = MigrateSem . Log.debug

    error msg = MigrateSem do
      Log.error msg
      stop (DbError.Table msg)
