module Polysemy.Hasql.Interpreter.DbTable where

import Control.Monad.Extra (andM)
import qualified Data.Map.Strict as Map
import Exon (exon)
import Generics.SOP (NP (Nil, (:*)))
import qualified Log
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import Sqel.Data.Dd (Dd, DdType)
import Sqel.Data.Migration (Mig (Mig), Migration (Migration, tableFrom), Migrations (Migrations), Migs, noMigrations)
import qualified Sqel.Data.PgType as PgType
import Sqel.Data.PgType (
  ColumnType (ColumnComp, ColumnPrim),
  PgColumn (PgColumn),
  PgColumnName (PgColumnName),
  PgColumns (PgColumns),
  PgComposite (PgComposite),
  PgPrimName (PgPrimName),
  PgTable (PgTable),
  PgTypeRef (PgTypeRef),
  )
import Sqel.Data.PgTypeName (PgTableName, pattern PgTypeName, PgTypeName)
import Sqel.Data.ProjectionSchema (ProjectionSchema)
import Sqel.Data.Sql (Sql)
import Sqel.Data.TableSchema (TableSchema (TableSchema))
import Sqel.PgType (CheckedProjection, projectionSchema)

import Polysemy.Hasql.Data.InitDb (ClientTag (ClientTag), InitDb (InitDb))
import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (Database)
import qualified Polysemy.Hasql.Effect.DbTable as DbTable
import Polysemy.Hasql.Effect.DbTable (DbTable)
import Polysemy.Hasql.Table (createTable, dbColumnsStatement, tableColumnsSql, typeColumnsSql)

-- TODO lots of duplication in this module
interpretDbTable ::
  ∀ d r .
  Members [Database !! DbError, Log, Embed IO] r =>
  TableSchema d ->
  InterpreterFor (DbTable d !! DbError) r
interpretDbTable (TableSchema table@PgTable {name = PgTypeName name} _ _) =
  interpretResumable \case
    DbTable.Statement q stmt ->
      restop (Database.withInit initDb (Database.statement q stmt))
  where
    initDb = InitDb (ClientTag name) True \ _ -> initTable table noMigrations

typeColumns ::
  Member Database r =>
  Sql ->
  PgTypeName table ->
  Sem r (Map PgColumnName (Either PgTypeRef PgPrimName))
typeColumns code (PgTypeName name) =
  Map.fromList . fmap mktype <$> Database.statement name (dbColumnsStatement code)
  where
    mktype = \case
      (col, "USER-DEFINED", n) ->
        (PgColumnName col, Left (PgTypeRef n))
      (col, n, _) ->
        (PgColumnName col, Right (PgPrimName n))

tableColumns ::
  Member Database r =>
  PgTableName ->
  Sem r (Map PgColumnName (Either PgTypeRef PgPrimName))
tableColumns =
  typeColumns tableColumnsSql

columnMap :: [PgColumn] -> Map PgColumnName ColumnType
columnMap =
  Map.fromList . fmap \ PgColumn {name, pgType} -> (name, pgType)

typeMatch ::
  Member Database r =>
  PgComposite ->
  Sem r Bool
typeMatch (PgComposite name (PgColumns cols)) = do
  dbCols <- typeColumns typeColumnsSql name
  pure (dbCols == colsByName)
  where
    colsByName = columnMap cols <&> \case
      ColumnPrim n _ _ -> Right n
      ColumnComp n _ _ -> Left n

tableMatch ::
  Member Database r =>
  PgTable d ->
  Sem r Bool
tableMatch (PgTable tableName (PgColumns cols) types _ _ _) = do
  dbCols <- tableColumns tableName
  andM (pure (dbCols == colsByName) : (typeMatch <$> Map.elems types))
  where
    colsByName = columnMap cols <&> \case
      ColumnPrim n _ _ -> Right n
      ColumnComp n _ _ -> Left n

startMigration ::
  Member Log r =>
  Migration (Sem r) ('Mig from to) ->
  Sem r ()
startMigration (Migration _ _ _ action) = do
  Log.info [exon|Starting migration|]
  action

-- TODO is it necessary for migrations to be stored as an NP? there's no type stuff going on here anymore
class RunMigrations d migs r where
  runMigrations ::
    PgTable d ->
    NP (Migration (Sem r)) migs ->
    Sem r ()

instance (
    Member (Stop DbError) r
  ) => RunMigrations d '[] r where
  runMigrations (PgTable (PgTypeName name) _ _ _ _ _) Nil =
    stop (DbError.Table [exon|No migration fits the current table layout for #{name}|])

instance (
    Members [Database, Log] r,
    RunMigrations d migs r
  ) => RunMigrations d ('Mig from to : migs) r where
  runMigrations table (h :* t) =
    ifM (tableMatch (tableFrom h)) (startMigration h) nextMigration
    where
      nextMigration = runMigrations table t *> startMigration h

initTable ::
  Members [Database, Stop DbError, Log, Embed IO] r =>
  RunMigrations d (Migs ds d) r =>
  PgTable d ->
  Migrations (Sem r) ds d ->
  Sem r ()
initTable table@PgTable {name = PgTypeName name} (Migrations migrations) =
  ifM (Map.null <$> tableColumns (table ^. #name))
  do
    Database.use \ c -> do
      Log.debug [exon|Creating nonexistent table #{name}|]
      createTable c table
  do
    unlessM (tableMatch table) do
      Log.debug [exon|Initiating migrations for table #{name}|]
      runMigrations table migrations

interpretTableMigrations ::
  ∀ d ds r .
  RunMigrations d (Migs ds d) (Database : Stop DbError : r) =>
  Members [Database !! DbError, Log, Embed IO] r =>
  TableSchema d ->
  Migrations (Sem (Database : Stop DbError : r)) ds d ->
  InterpreterFor (DbTable d !! DbError) r
interpretTableMigrations (TableSchema table@PgTable {name = PgTypeName name} _ _) migrations =
  interpretResumable \case
    DbTable.Statement q stmt -> do
      restop (Database.withInit initDb (Database.statement q stmt))
  where
    initDb = InitDb (ClientTag name) True \ _ -> initTable table migrations

interpretTableView ::
  Member (DbTable table !! DbError) r =>
  ProjectionSchema view table ->
  InterpreterFor (DbTable view !! DbError) r
interpretTableView _ =
  interpretResumable \case
    DbTable.Statement q stmt -> do
      restop (DbTable.statement q stmt)

interpretTableViewDd ::
  CheckedProjection view table =>
  Member (DbTable (DdType table) !! DbError) r =>
  Dd table ->
  Dd view ->
  InterpreterFor (DbTable (DdType view) !! DbError) r
interpretTableViewDd table view =
  interpretTableView (projectionSchema view table)
