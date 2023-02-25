module Sqel.Migration.Table where

import qualified Data.Map as Map

import Sqel.Data.Dd (Dd, DdType)
import qualified Sqel.Data.Migration as Migration
import Sqel.Data.Migration (Mig (Mig), Migration (Migration), MigrationActions (AutoActions))
import Sqel.Migration.Ddl (DdlTypes, ddTable)
import Sqel.Migration.Type (TableChange (tableChange), TypeChanges (typeChanges))
import Sqel.PgType (pgTable)
import Sqel.ReifyDd (ReifyDd)

class TableChanges old new where
  tableChanges :: Dd old -> Dd new -> MigrationActions ext

instance (
    DdlTypes 'True old (oldTable : oldTypes),
    DdlTypes 'True new (newTable : newTypes),
    TypeChanges oldTypes newTypes,
    TableChange oldTable newTable
  ) => TableChanges old new where
    tableChanges old new =
      AutoActions {
        table = tableChange oldTable newTable,
        types = Map.fromList (typeChanges oldTypes newTypes)
      }
      where
        (oldTable, oldTypes) = ddTable old
        (newTable, newTypes) = ddTable new

class MigrationTables m old new where
  withMigrationTables ::
    MigrationActions ext ->
    Dd old ->
    Dd new ->
    Migration ('Mig (DdType old) (DdType new) m ext)

instance (
    ReifyDd old,
    ReifyDd new
  ) => MigrationTables m old new where
    withMigrationTables actions old new =
      Migration (pgTable old) (pgTable new) actions

class AutoMigration old new where
  autoMigration :: Dd old -> Dd new -> Migration ('Mig (DdType old) (DdType new) m Void)

instance (
    TableChanges old new,
    ReifyDd old,
    ReifyDd new
  ) => AutoMigration old new where
    autoMigration old new =
      withMigrationTables (tableChanges old new) old new

migrateAuto ::
  AutoMigration old new =>
  Dd old ->
  Dd new ->
  Migration ('Mig (DdType old) (DdType new) m Void)
migrateAuto =
  autoMigration
