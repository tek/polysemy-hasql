module Polysemy.Hasql.Migration where

import Sqel.Migration (TableDdl)

import Polysemy.Hasql.Data.MigrateSem (MigrateSem)

type SemMigrations r tag table migs =
  TableDdl tag (MigrateSem r) table migs

-- type CustomSemMigrations r migs =
--   All (CustomMigration (MigrateSem r)) migs
