module Polysemy.Hasql.Interpreter.DbTable where

import Polysemy.Db.Data.DbError (DbError)
import Sqel (DdType, ResultShape, Statement)
import qualified Sqel.Data.Statement
import Sqel.Data.Statement (Statement (Statement))
import Sqel.Exts (pattern PgOnlyTableName, SqelFor)
import Sqel.Migration (noMigrations, runMigrations)
import Sqel.Migration.Run (RunMigrations, migrationTableName)

import Polysemy.Hasql.Data.InitDb (ClientTag (ClientTag), InitDb (InitDb))
import Polysemy.Hasql.Data.MigrateSem (unMigrateSem)
import Polysemy.Hasql.Data.SafeStatement (pattern SafeStatement, safeStatement)
import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (Database)
import qualified Polysemy.Hasql.Effect.DbTable as DbTable
import Polysemy.Hasql.Effect.DbTable (DbTable)
import Polysemy.Hasql.Migration (SemMigrations)

ratifyTable :: Statement (table : tables) query proj -> Statement tables query proj
ratifyTable Statement {..} = Statement {..}

withTable ::
  ∀ migs tag table r' r a .
  Members [Log, Embed IO] r =>
  Member Log r' =>
  RunMigrations tag table =>
  (∀ x . Sem (Database : Stop DbError : r') x -> Sem (Database : Stop DbError : r) x) ->
  SemMigrations r' tag table migs ->
  Sem (Stop DbError : r) a ->
  Sem (Stop DbError : Database !! DbError : r) a
withTable raiser migrations ma =
  restop (Database.withInit initDb (raise (raiseUnder ma)))
  where
    initDb :: InitDb (Sem (Database : Stop DbError : Database !! DbError : r))
    initDb =
      InitDb (ClientTag name) True \ _ ->
        void (raise2Under (raiser (unMigrateSem (runMigrations migrations))))
    PgOnlyTableName name = migrationTableName migrations

runStatement ::
  Members [Database !!DbError, Log, Embed IO] r =>
  Member Log r' =>
  ResultShape proj result =>
  RunMigrations tag table =>
  (∀ x . Sem (Database : Stop DbError : r') x -> Sem (Database : Stop DbError : r) x) ->
  SemMigrations r' tag table migs ->
  Bool ->
  query ->
  Statement '[DdType table] query proj ->
  Sem (Stop DbError : Database !! DbError : r) result
runStatement raiser migrations prep query stmt =
  withTable raiser migrations (restop (Database.statement query hs))
  where
    SafeStatement hs = safeStatement prep (ratifyTable stmt)

handleDbTable ::
  ∀ r0 migs tag table r' r a .
  Members [Database !!DbError, Log, Embed IO] r =>
  Member Log r' =>
  RunMigrations tag table =>
  (∀ x . Sem (Database : Stop DbError : r') x -> Sem (Database : Stop DbError : r) x) ->
  SemMigrations r' tag table migs ->
  DbTable (DdType table) (Sem r0) a ->
  Tactical (DbTable (DdType table) !! DbError) (Sem r0) (Stop DbError : r) a
handleDbTable raiser migrations = \case
  DbTable.WithTable stmt use ->
    subsume_ (withTable (insertAt @2 . raiser) migrations (raise (runTSimple (use (ratifyTable stmt)))))
  DbTable.Statement prep query stmt -> do
    pureT =<< subsume_ (runStatement raiser migrations prep query stmt)

interpretTable ::
  ∀ table tag r .
  RunMigrations tag table =>
  Members [Database !! DbError, Log, Embed IO] r =>
  SqelFor tag table ->
  InterpreterFor (DbTable (DdType table) !! DbError) r
interpretTable table =
  interpretResumableH \ sem -> handleDbTable id (noMigrations table) sem

tablesScope ::
  Member (Scoped p (Database !! DbError)) r =>
  p ->
  (() -> Sem (Database !! DbError : r) a) ->
  Sem r a
tablesScope conn use =
  scoped conn (use ())

interpretTableMigrations ::
  ∀ table migs tag r .
  RunMigrations tag table =>
  Members [Database !! DbError, Log, Embed IO] r =>
  SemMigrations r tag table migs ->
  InterpreterFor (DbTable (DdType table) !! DbError) r
interpretTableMigrations migrations =
  interpretResumableH \ sem ->
    handleDbTable id migrations sem

interpretTableMigrationsScoped ::
  RunMigrations tag table =>
  Members [Scoped p (Database !! DbError), Log, Embed IO] r =>
  SemMigrations r tag table migs ->
  InterpreterFor (Scoped p (DbTable (DdType table) !! DbError)) r
interpretTableMigrationsScoped migrations =
  interpretResumableScopedWithH @'[Database !! DbError] tablesScope \ () sem ->
    handleDbTable (insertAt @2) migrations sem

interpretTableMigrationsScoped' ::
  Member Log r' =>
  RunMigrations tag table =>
  Members [Scoped p (Database !! DbError), Log, Embed IO] r =>
  (∀ x . Sem (Database : Stop DbError : r') x -> Sem (Database : Stop DbError : r) x) ->
  SemMigrations r' tag table migs ->
  InterpreterFor (Scoped p (DbTable (DdType table) !! DbError)) r
interpretTableMigrationsScoped' raiser migrations =
  interpretResumableScopedWithH @'[Database !! DbError] tablesScope \ () sem ->
    handleDbTable (insertAt @2 . raiser) migrations sem

interpretTablesMigrations ::
  ∀ table migs tag p r .
  RunMigrations tag table =>
  Members [Scoped p (Database !! DbError), Database !! DbError, Log, Embed IO] r =>
  SemMigrations r tag table migs ->
  InterpretersFor [Scoped p (DbTable (DdType table) !! DbError), DbTable (DdType table) !! DbError] r
interpretTablesMigrations migrations =
  interpretTableMigrations migrations .
  interpretTableMigrationsScoped' raise2Under migrations

interpretTables ::
  RunMigrations tag table =>
  Members [Scoped p (Database !! DbError), Database !! DbError, Log, Embed IO] r =>
  SqelFor tag table ->
  InterpretersFor [Scoped p (DbTable (DdType table) !! DbError), DbTable (DdType table) !! DbError] r
interpretTables table =
  interpretTablesMigrations (noMigrations table)
