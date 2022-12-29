module Polysemy.Hasql.Interpreter.Store where

import Generics.SOP (NP (Nil))
import Hasql.Connection (Connection)
import Hasql.Statement (Statement)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (QStore, Store)
import Sqel.Data.Dd (
  Comp (Prod),
  CompInc (Nest),
  Dd (Dd),
  DdInc (DdNest),
  DdK (DdK),
  DdStruct (DdComp),
  DdVar (DdProd),
  ProdType (Reg),
  Struct (Comp, Prim),
  )
import Sqel.Data.Migration (noMigrations)
import Sqel.Data.Mods (pattern NoMods, NoMods)
import qualified Sqel.Data.PgType as PgType
import Sqel.Data.PgType (PgTable (PgTable))
import Sqel.Data.PgTypeName (pattern PgTypeName)
import Sqel.Data.QuerySchema (QuerySchema, emptyQuerySchema)
import Sqel.Data.Sel (Sel (SelAuto, SelSymbol), SelW (SelWAuto, SelWSymbol))
import qualified Sqel.Data.TableSchema as TableSchema
import Sqel.Data.TableSchema (TableSchema (TableSchema))
import Sqel.Data.Uid (Uid)
import Sqel.Prim (primAs)
import Sqel.ResultShape (ResultShape)
import Sqel.Statement (delete, insert, selectWhere, upsert)

import Polysemy.Hasql.Data.InitDb (ClientTag (ClientTag), InitDb (InitDb))
import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (ConnectionSource, Database)
import qualified Polysemy.Hasql.Effect.DbTable as DbTable
import Polysemy.Hasql.Effect.DbTable (DbTable, StoreTable)
import Polysemy.Hasql.Interpreter.DbTable (initTable)

type EmptyQuery =
  'DdK ('SelSymbol "") NoMods () ('Comp 'SelAuto ('Prod 'Reg) 'Nest '[])

emptyQuery :: Dd EmptyQuery
emptyQuery =
  Dd (SelWSymbol Proxy) NoMods (DdComp SelWAuto DdProd DdNest Nil)

primIdQuery :: Dd ('DdK ('SelSymbol "id") NoMods a 'Prim)
primIdQuery =
  primAs @"id"

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
    is = insert table
    us = upsert table
    ds :: Statement q (f d)
    ds = delete query table
    qs :: Statement q (f d)
    qs = selectWhere query table
    qas :: Statement () [d]
    qas = selectWhere emptyQuerySchema table
    das :: Statement () [d]
    das = delete emptyQuerySchema table

interpretStoreDb ::
  ∀ i d e r .
  Member (StoreTable i d !! e) r =>
  TableSchema (Uid i d) ->
  QuerySchema i (Uid i d) ->
  InterpreterFor (Store i d !! e) r
interpretStoreDb table query =
  interpretQStoreDb table query

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
-- TODO local DbTable?
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
    is = insert schema
    qs :: Statement i (Maybe (Uid i d))
    qs = selectWhere query schema
    qas :: Statement () [Uid i d]
    qas = selectWhere emptyQuerySchema schema
    initDb ::
      Members [Database, Stop DbError, Log, Embed IO] r' =>
      InitDb (Sem r')
    initDb = InitDb (ClientTag name) True \ _ -> initTable table noMigrations
