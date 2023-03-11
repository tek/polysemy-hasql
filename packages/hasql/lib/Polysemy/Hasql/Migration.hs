module Polysemy.Hasql.Migration where

import Generics.SOP (All)
import Polysemy.Db.Data.DbError (DbError)
import Sqel.Data.Migration (CustomMigration, HoistMigrations (hoistMigrations), Migrations)

import Polysemy.Hasql.Data.MigrateSem (MigrateSem (MigrateSem), unMigrateSem)
import Polysemy.Hasql.Effect.Database (Database)

type SemMigrations r migs = Migrations (MigrateSem r) migs

type HoistSemMigrations extra r migs migs' =
  HoistMigrations (MigrateSem r) (MigrateSem (extra ++ r)) migs migs'

type CustomSemMigrations r migs =
  All (CustomMigration (MigrateSem r)) migs

hoistSemMigrations ::
  ∀ extra r migs migs' .
  HoistSemMigrations extra r migs migs' =>
  (∀ x . Sem (Database : Stop DbError : r) x -> Sem (Database : Stop DbError : extra ++ r) x) ->
  SemMigrations r migs ->
  SemMigrations (extra ++ r) migs'
hoistSemMigrations f m =
  hoistMigrations (MigrateSem . f . unMigrateSem) m
