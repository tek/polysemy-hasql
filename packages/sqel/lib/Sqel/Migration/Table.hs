module Sqel.Migration.Table where

import qualified Sqel.Data.Dd as Dd
import Sqel.Data.Dd (Dd)
import Sqel.Data.Migration (Mig (Mig), Migration (Migration), MigrationAction)
import Sqel.Data.PgType (PgTable)
import Sqel.Migration.Ddl (DdlTypes, ddTable)
import Sqel.Migration.Type (TypeChange (typeChange), TypeChanges (changes))
import Sqel.PgType (pgTable)
import Sqel.ReifyDd (ReifyDd)

class TableChanges old new where
  tableChanges :: Dd old -> Dd new -> [MigrationAction]

instance (
    DdlTypes 'True old (oldTable : oldTypes),
    DdlTypes 'True new (newTable : newTypes),
    TypeChanges oldTypes newTypes,
    TypeChange oldTable newTable
  ) => TableChanges old new where
    tableChanges old new =
      typeChange oldTable newTable <> changes oldTypes newTypes
      where
        (oldTable, oldTypes) = ddTable old
        (newTable, newTypes) = ddTable new

class AutoMigrationEffect m where
  autoMigrationEffect :: PgTable a -> PgTable b -> [MigrationAction] -> m ()

class DdMigrations m old new where
  ddMigrations :: Dd old -> Dd new -> Migration m ('Mig (Dd.DdType old) (Dd.DdType new))

instance (
  TableChanges old new,
  ReifyDd old,
  ReifyDd new,
  AutoMigrationEffect m
  ) => DdMigrations m old new where
    ddMigrations old new =
      Migration tableOld tableNew actions (autoMigrationEffect tableOld tableNew actions)
      where
        tableOld = pgTable old
        tableNew = pgTable new
        actions = tableChanges old new

migrateAuto ::
  DdMigrations m old new =>
  Dd old ->
  Dd new ->
  Migration m ('Mig (Dd.DdType old) (Dd.DdType new))
migrateAuto =
  ddMigrations
