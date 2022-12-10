module Polysemy.Hasql.Interpreter.Store where

import Conc (interpretScopedRWith)
import Control.Monad.Extra (andM)
import qualified Data.Map.Strict as Map
import Exon (exon)
import Generics.SOP (NP (Nil, (:*)))
import Hasql.Connection (Connection)
import Hasql.Statement (Statement)
import qualified Log
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store, QStore)
import Sqel.Data.Dd (
  Comp (Prod),
  CompInc (Nest),
  Dd (Dd),
  DdInc (DdNest),
  DdK (DdK),
  DdStruct (DdComp),
  DdVar (DdProd),
  ProdType (Reg),
  Struct (Comp),
  )
import Sqel.Data.Migration (Mig (Mig), Migration (Migration, tableFrom), Migrations (Migrations), Migs, noMigrations)
import Sqel.Data.Mods (pattern NoMods, NoMods)
import qualified Sqel.Data.PgType as PgType
import Sqel.Data.PgType (
  ColumnType (ColumnComp, ColumnPrim),
  PgColumnName (PgColumnName),
  PgColumns (PgColumns),
  PgComposite (PgComposite),
  PgPrimName (PgPrimName),
  PgTable (PgTable),
  PgTypeRef (PgTypeRef),
  )
import Sqel.Data.PgTypeName (PgTableName, pattern PgTypeName, PgTypeName)
import Sqel.Data.QuerySchema (QuerySchema, emptyQuerySchema)
import Sqel.Data.Sel (Sel (SelAuto, SelSymbol), SelW (SelWAuto, SelWSymbol))
import Sqel.Data.Sql (Sql)
import qualified Sqel.Data.TableSchema as TableSchema
import Sqel.Data.TableSchema (TableSchema (TableSchema))
import Sqel.Data.Uid (Uid)
import Sqel.Statement (dStatement, iStatement, qStatement, uStatement)

import Polysemy.Hasql.Data.InitDb (ClientTag (ClientTag), InitDb (InitDb))
import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (ConnectionSource, Database)
import qualified Polysemy.Hasql.Effect.DbTable as DbTable
import Polysemy.Hasql.Effect.DbTable (DbTable, StoreTable)
import Polysemy.Hasql.Table (createTable, dbColumnsStatement, tableColumnsSql, typeColumnsSql)
import Sqel.ResultShape (ResultShape)

type EmptyQuery =
  'DdK ('SelSymbol "") NoMods () ('Comp 'SelAuto ('Prod 'Reg) 'Nest '[])

emptyQuery :: Dd EmptyQuery
emptyQuery =
  Dd (SelWSymbol Proxy) NoMods (DdComp SelWAuto DdProd DdNest Nil)

type NoResult =
  'DdK ('SelSymbol "") NoMods () ('Comp ('SelSymbol "") ('Prod 'Reg) 'Nest '[])

noResult :: Dd NoResult
noResult =
  Dd (SelWSymbol Proxy) NoMods (DdComp (SelWSymbol Proxy) DdProd DdNest Nil)

interpretQStoreDb ::
  ∀ f q d e r .
  ResultShape d (f d) =>
  Member (DbTable d !! e) r =>
  TableSchema d ->
  QuerySchema q d ->
  InterpreterFor (QStore f q d !! e) r
interpretQStoreDb table query =
  interpretResumable \case
    Store.Insert d ->
      restop (DbTable.statement d is)
    Store.Upsert d ->
      restop (DbTable.statement d us)
    Store.Delete i ->
      restop (DbTable.statement i ds)
    Store.DeleteAll ->
      restop (DbTable.statement () das)
    Store.Fetch i ->
      restop (DbTable.statement i qs)
    Store.FetchAll ->
      restop (DbTable.statement () qas)
  where
    is = iStatement table
    us = uStatement table
    ds :: Statement q (f d)
    ds = dStatement query table
    qs :: Statement q (f d)
    qs = qStatement query table
    qas :: Statement () [d]
    qas = qStatement emptyQuerySchema table
    das :: Statement () [d]
    das = dStatement emptyQuerySchema table

interpretStoreDb ::
  ∀ i d e r .
  Member (StoreTable i d !! e) r =>
  TableSchema (Uid i d) ->
  QuerySchema i (Uid i d) ->
  InterpreterFor (Store i d !! e) r
interpretStoreDb table query =
  interpretQStoreDb table query

-- TODO lots of duplication in this module
interpretDbTable ::
  ∀ d r .
  Members [Database !! DbError, Log, Embed IO] r =>
  TableSchema d ->
  InterpreterFor (DbTable d !! DbError) r
interpretDbTable schema@(TableSchema table@PgTable {name = PgTypeName name} _ _) =
  interpretResumable \case
    DbTable.Schema ->
      pure schema
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

typeMatch ::
  Member Database r =>
  PgComposite ->
  Sem r Bool
typeMatch (PgComposite name (PgColumns cols)) = do
  dbCols <- typeColumns typeColumnsSql name
  pure (dbCols == colsByName)
  where
    colsByName = Map.fromList cols <&> \case
      ColumnPrim n _ -> Right n
      ColumnComp n -> Left n

tableMatch ::
  Member Database r =>
  PgTable d ->
  Sem r Bool
tableMatch (PgTable tableName (PgColumns cols) types _ _ _) = do
  dbCols <- tableColumns tableName
  andM (pure (dbCols == colsByName) : (typeMatch <$> Map.elems types))
  where
    colsByName = Map.fromList cols <&> \case
      ColumnPrim n _ -> Right n
      ColumnComp n -> Left n

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
initTable table (Migrations migrations) =
  ifM (Map.null <$> tableColumns (table ^. #name))
  do
    Database.use \ c -> createTable c table
  do
    unlessM (tableMatch table) do
      runMigrations table migrations

interpretTableMigrations ::
  ∀ d ds r .
  RunMigrations d (Migs ds d) (Database : Stop DbError : r) =>
  Members [Database !! DbError, Log, Embed IO] r =>
  TableSchema d ->
  Migrations (Sem (Database : Stop DbError : r)) ds d ->
  InterpreterFor (DbTable d !! DbError) r
interpretTableMigrations schema@(TableSchema table@PgTable {name = PgTypeName name} _ _) migrations =
  interpretResumable \case
    DbTable.Schema ->
      pure schema
    DbTable.Statement q stmt -> do
      restop (Database.withInit initDb (Database.statement q stmt))
  where
    initDb = InitDb (ClientTag name) True \ _ -> initTable table migrations

tableDatabaseScoped ::
  Members [Scoped conn (Database !! DbError), Stop DbError, Log, Embed IO] r =>
  conn ->
  InterpreterFor (Database !! DbError) r
tableDatabaseScoped conn ma =
  scoped conn do
    restop (raise ma)

storeScope ::
  Members [Scoped ConnectionSource (Database !! DbError), Log, Embed IO] r =>
  Connection ->
  (() -> Sem (Database !! DbError : Stop DbError : r) a) ->
  Sem (Stop DbError : r) a
storeScope conn use =
  tableDatabaseScoped (Database.Supplied "transaction" conn) do
    insertAt @1 (use ())

-- TODO move withInit to scope?
interpretStoreXa ::
  ∀ i d r .
  Members [Scoped ConnectionSource (Database !! DbError), Log, Embed IO] r =>
  TableSchema (Uid i d) ->
  QuerySchema i (Uid i d) ->
  InterpreterFor (Scoped Connection (Store i d !! DbError) !! DbError) r
interpretStoreXa schema@(TableSchema {pg = table@PgTable {name = PgTypeName name}}) query =
  interpretScopedRWith @'[Database !! DbError] storeScope \ () -> \case
    Store.Insert d ->
      restop (Database.withInit initDb (Database.statement d is))
    Store.Fetch i ->
      restop (Database.withInit initDb (Database.statement i qs))
    Store.FetchAll ->
      restop (Database.withInit initDb (Database.statement () qas))
    _ ->
      undefined
  where
    is = iStatement schema
    qs :: Statement i (Maybe (Uid i d))
    qs = qStatement query schema
    qas :: Statement () [Uid i d]
    qas = qStatement emptyQuerySchema schema
    initDb :: InitDb (Sem (Database : Stop DbError : Database !! DbError : Stop DbError : r))
    initDb = InitDb (ClientTag name) True \ _ -> initTable table noMigrations
