module Polysemy.Hasql.Interpreter.DbTable where

import Polysemy.Db.Data.DbError (DbError)
import Sqel.Data.Dd (Dd, DdType)
import Sqel.Data.Migration (noMigrations)
import qualified Sqel.Data.PgType as PgType
import Sqel.Data.PgType (PgTable (PgTable))
import Sqel.Data.PgTypeName (pattern PgTypeName)
import Sqel.Data.ProjectionWitness (ProjectionWitness)
import Sqel.Data.TableSchema (TableSchema (TableSchema))
import Sqel.Migration.Run (runMigrations)
import Sqel.PgType (CheckedProjection, projectionWitness)

import Polysemy.Hasql.Data.InitDb (ClientTag (ClientTag), InitDb (InitDb))
import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (Database)
import qualified Polysemy.Hasql.Effect.DbTable as DbTable
import Polysemy.Hasql.Effect.DbTable (DbTable)
import Polysemy.Hasql.Migration (CustomSemMigrations, MigrateSem (unMigrateSem), SemMigrations)

handleDbTable ::
  ∀ d migs m r' r a .
  CustomSemMigrations r' migs =>
  Members [Log, Embed IO] r =>
  Member Log r' =>
  (∀ x . Sem (Database : Stop DbError : r') x -> Sem (Database : Stop DbError : r) x) ->
  TableSchema d ->
  SemMigrations r' migs ->
  DbTable d m a ->
  Sem (Stop DbError : Database !! DbError : r) a
handleDbTable raiser (TableSchema table@PgTable {name = PgTypeName name} _ _) migrations = \case
  DbTable.Statement q stmt ->
    restop (Database.withInit initDb (Database.statement q stmt))
  where
    initDb :: InitDb (Sem (Database : Stop DbError : Database !! DbError : r))
    initDb =
      InitDb (ClientTag name) True \ _ ->
        void (raise2Under (raiser (unMigrateSem (runMigrations table migrations))))

interpretTable ::
  ∀ d r .
  Members [Database !! DbError, Log, Embed IO] r =>
  TableSchema d ->
  InterpreterFor (DbTable d !! DbError) r
interpretTable schema =
  interpretResumable (subsume . subsume . raise2Under . handleDbTable id schema noMigrations)

tablesScope ::
  Member (Scoped p (Database !! DbError)) r =>
  p ->
  (() -> Sem (Database !! DbError : r) a) ->
  Sem r a
tablesScope conn use =
  scoped conn (use ())

interpretTableMigrations ::
  ∀ d migs r .
  CustomSemMigrations r migs =>
  Members [Database !! DbError, Log, Embed IO] r =>
  TableSchema d ->
  SemMigrations r migs ->
  InterpreterFor (DbTable d !! DbError) r
interpretTableMigrations schema migrations =
  interpretResumable \ ma ->
    subsume_ (handleDbTable id schema migrations ma)

interpretTableMigrationsScoped ::
  CustomSemMigrations r migs =>
  Members [Scoped p (Database !! DbError), Log, Embed IO] r =>
  TableSchema d ->
  SemMigrations r migs ->
  InterpreterFor (Scoped p (DbTable d !! DbError)) r
interpretTableMigrationsScoped schema migrations =
  interpretResumableScopedWith @'[Database !! DbError] tablesScope \ () -> handleDbTable id schema migrations

interpretTableMigrationsScoped' ::
  CustomSemMigrations r' migs =>
  Member Log r' =>
  Members [Scoped p (Database !! DbError), Log, Embed IO] r =>
  (∀ x . Sem (Database : Stop DbError : r') x -> Sem (Database : Stop DbError : r) x) ->
  TableSchema d ->
  SemMigrations r' migs ->
  InterpreterFor (Scoped p (DbTable d !! DbError)) r
interpretTableMigrationsScoped' raiser schema migrations =
  interpretResumableScopedWith @'[Database !! DbError] tablesScope \ () -> handleDbTable raiser schema migrations

interpretTablesMigrations ::
  ∀ d migs p r .
  CustomSemMigrations r migs =>
  Members [Scoped p (Database !! DbError), Database !! DbError, Log, Embed IO] r =>
  TableSchema d ->
  SemMigrations r migs ->
  InterpretersFor [Scoped p (DbTable d !! DbError), DbTable d !! DbError] r
interpretTablesMigrations schema migrations =
  interpretTableMigrations schema migrations .
  interpretTableMigrationsScoped' raise2Under schema migrations

interpretTables ::
  Members [Scoped p (Database !! DbError), Database !! DbError, Log, Embed IO] r =>
  TableSchema d ->
  InterpretersFor [Scoped p (DbTable d !! DbError), DbTable d !! DbError] r
interpretTables schema =
  interpretTablesMigrations schema noMigrations

interpretTableView ::
  Member (DbTable table !! DbError) r =>
  ProjectionWitness view table ->
  InterpreterFor (DbTable view !! DbError) r
interpretTableView _ =
  interpretResumable \case
    DbTable.Statement q stmt ->
      restop (DbTable.statement q stmt)

interpretTableViewDd ::
  CheckedProjection view table =>
  Member (DbTable (DdType table) !! DbError) r =>
  Dd table ->
  Dd view ->
  InterpreterFor (DbTable (DdType view) !! DbError) r
interpretTableViewDd table view =
  interpretTableView (projectionWitness view table)
