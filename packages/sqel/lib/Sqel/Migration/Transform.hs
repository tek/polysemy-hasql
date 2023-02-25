module Sqel.Migration.Transform where

import qualified Data.Map as Map
import Hasql.Statement (Statement)

import Sqel (MkTableSchema (tableSchema))
import Sqel.Class.MigrationEffect (MigrationEffect (runStatement, runStatement_))
import Sqel.Data.Dd (Dd, DdType)
import qualified Sqel.Data.Migration as Migration
import Sqel.Data.Migration (
  CompAction,
  CustomMigration (customMigration),
  Mig (Mig),
  Migration,
  MigrationActions (CustomActions),
  )
import Sqel.Data.PgTypeName (PgCompName, pattern PgTypeName)
import Sqel.Data.SqlFragment (Select (Select))
import Sqel.Data.Sql (sql, toSql)
import Sqel.Data.SqlFragment (Insert (Insert))
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Migration.Ddl (DdlTypes, ddTable)
import Sqel.Migration.Run (autoKeys, runTypesMigration)
import Sqel.Migration.Table (MigrationTables (withMigrationTables))
import Sqel.Migration.Type (TypeChanges (typeChanges))
import Sqel.ReifyDd (ReifyDd)
import Sqel.Sql.Type (createTable)
import Sqel.Statement (plain, prepared, unprepared)

data MigrateTransform m old new =
  MigrateTransform {
    trans :: [old] -> m [new],
    types :: Map PgCompName CompAction,
    schemaOld :: TableSchema old,
    schemaNew :: TableSchema new
  }

class MkMigrateTransform m old new where
  migrateTransform ::
    Dd old ->
    Dd new ->
    ([DdType old] -> m [DdType new]) ->
    Migration ('Mig (DdType old) (DdType new) m (MigrateTransform m (DdType old) (DdType new)))

instance (
    DdlTypes 'True old (oldTable : oldTypes),
    DdlTypes 'True new (newTable : newTypes),
    TypeChanges oldTypes newTypes,
    MkTableSchema old,
    MkTableSchema new,
    ReifyDd old,
    ReifyDd new
  ) => MkMigrateTransform m old new where
    migrateTransform old new f =
      withMigrationTables (CustomActions actions) old new
      where
        actions =
          MigrateTransform {
            trans = f,
            types = Map.fromList (typeChanges oldTypes newTypes),
            ..
          }
        schemaOld = tableSchema old
        schemaNew = tableSchema new
        (_, oldTypes) = ddTable old
        (_, newTypes) = ddTable new

transformAndMigrate ::
  ∀ old new m .
  Monad m =>
  MigrationEffect m =>
  Set PgCompName ->
  MigrateTransform m old new ->
  m ()
transformAndMigrate eligible MigrateTransform {..} = do
  oldRows <- runStatement () fetchOld
  newRows <- trans oldRows
  runTypesMigration eligible types
  runPlain [sql|alter table ##{schemaOld ^. #pg . #name} rename to "##{oldName}-migration-temp"|]
  runPlain (createTable (schemaNew ^. #pg))
  for_ newRows \ row -> runStatement_ row insertNew
  where
    PgTypeName oldName = schemaOld ^. #pg . #name
    runPlain = runStatement_ () . plain
    fetchOld :: Statement () [old]
    fetchOld = unprepared [sql|##{Select schemaOld}|] (schemaOld ^. #decoder) mempty
    insertNew :: Statement new ()
    insertNew = prepared (toSql (Insert (schemaNew ^. #pg))) unit (schemaNew ^. #encoder)

instance (
    Monad m,
    MigrationEffect m
  ) => CustomMigration m ('Mig old new m (MigrateTransform m old new)) where
    customMigration _ =
      transformAndMigrate

    customTypeKeys MigrateTransform {types} =
      pure (autoKeys types)
