module Polysemy.Hasql.Interpreter.DbTable where

import Control.Monad.Extra (andM)
import qualified Data.Map.Strict as Map
import Exon (exon)
import Generics.SOP (NP (Nil, (:*)), SListI)
import qualified Log
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import Prettyprinter (Pretty, pretty, vsep, (<+>))
import Sqel.Data.Dd (Dd, DdType)
import Sqel.Data.Migration (
  Mig (Mig),
  Migration (Migration, tableFrom),
  Migrations (Migrations),
  Migs,
  hoistMigrations,
  noMigrations,
  )
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

handleDbTable ::
  Members [Database !! DbError, Log, Embed IO] r =>
  RunMigrations d (Migs ds d) =>
  TableSchema d ->
  Migrations (Sem (Database : Stop DbError : r)) ds d ->
  DbTable d m a ->
  Sem (Stop DbError : r) a
handleDbTable (TableSchema table@PgTable {name = PgTypeName name} _ _) migrations = \case
  DbTable.Statement q stmt ->
    restop (Database.withInit initDb (Database.statement q stmt))
  where
    initDb = InitDb (ClientTag name) True \ _ -> initTable table migrations

interpretDbTable ::
  ∀ d r .
  Members [Database !! DbError, Log, Embed IO] r =>
  TableSchema d ->
  InterpreterFor (DbTable d !! DbError) r
interpretDbTable schema =
  interpretResumable (handleDbTable schema noMigrations)

tablesScope ::
  Member (Scoped p (Database !! DbError)) r =>
  p ->
  (() -> Sem (Database !! DbError : r) a) ->
  Sem r a
tablesScope conn use =
  scoped conn (use ())

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

newtype PrettyColMap =
  PrettyColMap { unPrettyColMap :: Map PgColumnName (Either PgTypeRef PgPrimName) }
  deriving stock (Eq, Show, Generic)

instance Pretty PrettyColMap where
  pretty (PrettyColMap cols) =
    vsep (uncurry col <$> Map.toList cols)
    where
      col name = \case
        Right tpe -> "*" <+> pretty name <+> pretty tpe
        Left ref -> "+" <+> pretty name <+> pretty ref

columnMap :: [PgColumn] -> Map PgColumnName ColumnType
columnMap =
  Map.fromList . fmap \ PgColumn {name, pgType} -> (name, pgType)

typeMatch ::
  Members [Database, Log] r =>
  PgComposite ->
  Sem r Bool
typeMatch (PgComposite name (PgColumns cols)) = do
  dbCols <- typeColumns typeColumnsSql name
  Log.debug [exon|Trying type with:
#{show (pretty (PrettyColMap colsByName))}
for existing type with
#{show (pretty (PrettyColMap dbCols))}|]
  pure (dbCols == colsByName)
  where
    colsByName = columnMap cols <&> \case
      ColumnPrim n _ _ -> Right n
      ColumnComp n _ _ -> Left n

tableMatch ::
  Members [Database, Log] r =>
  PgTable d ->
  Sem r Bool
tableMatch (PgTable tableName (PgColumns cols) types _ _ _) = do
  dbCols <- tableColumns tableName
  Log.debug [exon|Trying table with:
#{show (pretty (PrettyColMap colsByName))}
for existing table with
#{show (pretty (PrettyColMap dbCols))}|]
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
class RunMigrations d migs where
  runMigrations ::
    Members [Database, Stop DbError, Log] r =>
    PgTable d ->
    NP (Migration (Sem r)) migs ->
    Sem r ()

instance RunMigrations d '[] where
  runMigrations (PgTable (PgTypeName name) _ _ _ _ _) Nil = do
    Log.error [exon|No migration fits the current table layout for #{name}|]
    stop (DbError.Table [exon|No migration fits the current table layout for #{name}|])

instance (
    RunMigrations d migs
  ) => RunMigrations d ('Mig from to : migs) where
  runMigrations table (h :* t) =
    ifM (tableMatch (tableFrom h)) (startMigration h) nextMigration
    where
      nextMigration = runMigrations table t *> startMigration h


initTable ::
  Members [Database, Stop DbError, Log, Embed IO] r =>
  RunMigrations d (Migs ds d) =>
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
  RunMigrations d (Migs ds d) =>
  Members [Database !! DbError, Log, Embed IO] r =>
  TableSchema d ->
  Migrations (Sem (Database : Stop DbError : r)) ds d ->
  InterpreterFor (DbTable d !! DbError) r
interpretTableMigrations schema migrations =
  interpretResumable \ ma ->
    handleDbTable schema migrations ma

interpretTablesMigrations ::
  ∀ d ds p r .
  SListI (Migs ds d) =>
  RunMigrations d (Migs ds d) =>
  Members [Scoped p (Database !! DbError), Database !! DbError, Log, Embed IO] r =>
  TableSchema d ->
  Migrations (Sem (Database : Stop DbError : r)) ds d ->
  InterpretersFor [Scoped p (DbTable d !! DbError), DbTable d !! DbError] r
interpretTablesMigrations schema migrations =
  interpretTableMigrations schema migrations .
  interpretResumableScopedWith @'[_] tablesScope \ () -> handleDbTable schema (hoistMigrations (insertAt @2) migrations)

interpretTables ::
  Members [Scoped p (Database !! DbError), Database !! DbError, Log, Embed IO] r =>
  TableSchema d ->
  InterpretersFor [Scoped p (DbTable d !! DbError), DbTable d !! DbError] r
interpretTables schema =
  interpretTablesMigrations schema noMigrations

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
