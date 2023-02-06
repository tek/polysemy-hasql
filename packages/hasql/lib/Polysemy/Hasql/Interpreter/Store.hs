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
import Sqel.Data.Mods (pattern NoMods, NoMods)
import Sqel.Data.QuerySchema (QuerySchema, emptyQuerySchema)
import Sqel.Data.Sel (Sel (SelSymbol), SelPrefix (DefaultPrefix), SelW (SelWSymbol), TSel (TSel), mkTSel)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Data.Uid (Uid)
import Sqel.Prim (primAs)
import Sqel.ResultShape (ResultShape)
import Sqel.Statement (delete, insert, selectWhere, upsert)

import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (ConnectionSource)
import qualified Polysemy.Hasql.Effect.DbTable as DbTable
import Polysemy.Hasql.Effect.DbTable (DbTable, StoreTable)

type EmptyQuery =
  'DdK ('SelSymbol "") NoMods () ('Comp ('TSel 'DefaultPrefix "") ('Prod 'Reg) 'Nest '[])

emptyQuery :: Dd EmptyQuery
emptyQuery =
  Dd (SelWSymbol Proxy) NoMods (DdComp mkTSel DdProd DdNest Nil)

primIdQuery :: Dd ('DdK ('SelSymbol "id") NoMods a 'Prim)
primIdQuery =
  primAs @"id"

type NoResult =
  'DdK ('SelSymbol "") NoMods () ('Comp ('TSel 'DefaultPrefix "") ('Prod 'Reg) 'Nest '[])

noResult :: Dd NoResult
noResult =
  Dd (SelWSymbol Proxy) NoMods (DdComp mkTSel DdProd DdNest Nil)

handleQStoreDb ::
  ∀ f q d e r m a .
  ResultShape d (f d) =>
  Members [DbTable d !! e, Stop e] r =>
  TableSchema d ->
  QuerySchema q d ->
  QStore f q d m a ->
  Sem r a
handleQStoreDb table query = \case
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

interpretQStoreDb ::
  ∀ f q d e r .
  ResultShape d (f d) =>
  Member (DbTable d !! e) r =>
  TableSchema d ->
  QuerySchema q d ->
  InterpreterFor (QStore f q d !! e) r
interpretQStoreDb table query =
  interpretResumable (handleQStoreDb table query)

interpretStoreDb ::
  ∀ i d e r .
  Member (StoreTable i d !! e) r =>
  TableSchema (Uid i d) ->
  QuerySchema i (Uid i d) ->
  InterpreterFor (Store i d !! e) r
interpretStoreDb table query =
  interpretQStoreDb table query

storeScope ::
  Members [Scoped ConnectionSource (DbTable d !! DbError), Log, Embed IO] r =>
  Connection ->
  (() -> Sem (DbTable d !! DbError : Stop DbError : r) a) ->
  Sem (Stop DbError : r) a
storeScope conn use =
  scoped (Database.Supplied "transaction" conn) (use ())

interpretQStoreXa ::
  ∀ f i d r .
  ResultShape d (f d) =>
  Members [Scoped ConnectionSource (DbTable d !! DbError), Log, Embed IO] r =>
  TableSchema d ->
  QuerySchema i d ->
  InterpreterFor (Scoped Connection (QStore f i d !! DbError) !! DbError) r
interpretQStoreXa table query =
  interpretScopedRWith @'[DbTable d !! DbError] storeScope \ () -> handleQStoreDb table query

interpretStoreXa ::
  ∀ i d r .
  Members [Scoped ConnectionSource (DbTable (Uid i d) !! DbError), Log, Embed IO] r =>
  TableSchema (Uid i d) ->
  QuerySchema i (Uid i d) ->
  InterpreterFor (Scoped Connection (Store i d !! DbError) !! DbError) r
interpretStoreXa =
  interpretQStoreXa @Maybe

interpretQStores ::
  ∀ f q d r .
  ResultShape d (f d) =>
  Members [Scoped ConnectionSource (DbTable d !! DbError), DbTable d !! DbError, Log, Embed IO] r =>
  TableSchema d ->
  QuerySchema q d ->
  InterpretersFor [QStore f q d !! DbError, Scoped Connection (QStore f q d !! DbError) !! DbError] r
interpretQStores table query =
  interpretQStoreXa table query .
  interpretQStoreDb table query

interpretStores ::
  ∀ i d r .
  Members [Scoped ConnectionSource (DbTable (Uid i d) !! DbError), StoreTable i d !! DbError, Log, Embed IO] r =>
  TableSchema (Uid i d) ->
  QuerySchema i (Uid i d) ->
  InterpretersFor [Store i d !! DbError, Scoped Connection (Store i d !! DbError) !! DbError] r
interpretStores =
  interpretQStores @Maybe
