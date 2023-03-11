module Polysemy.Hasql.Interpreter.Store where

import Hasql.Connection (Connection)
import Hasql.Statement (Statement)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (QStore, Store)
import Sqel (QuerySchema, emptyQuerySchema)
import Sqel (TableSchema)
import Sqel (Uid)
import Sqel.PgType (toFullProjection)
import Sqel.ResultShape (ResultShape)
import Sqel.Statement (delete, insert, selectWhere, upsert)

import Polysemy.Hasql.Effect.Database (ConnectionSource)
import qualified Polysemy.Hasql.Effect.DbTable as DbTable
import Polysemy.Hasql.Effect.DbTable (DbTable, StoreTable)
import Polysemy.Hasql.Transaction (interpretForXa)

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
    qs = selectWhere query full
    qas :: Statement () [d]
    qas = selectWhere emptyQuerySchema full
    das :: Statement () [d]
    das = delete emptyQuerySchema table
    full = toFullProjection table

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

interpretQStoreXa ::
  ∀ f err i d r .
  ResultShape d (f d) =>
  Members [Scoped ConnectionSource (DbTable d !! err), Log, Embed IO] r =>
  TableSchema d ->
  QuerySchema i d ->
  InterpreterFor (Scoped Connection (QStore f i d !! err) !! err) r
interpretQStoreXa table query =
  interpretForXa (handleQStoreDb table query)

interpretStoreXa ::
  ∀ err i d r .
  Members [Scoped ConnectionSource (DbTable (Uid i d) !! err), Log, Embed IO] r =>
  TableSchema (Uid i d) ->
  QuerySchema i (Uid i d) ->
  InterpreterFor (Scoped Connection (Store i d !! err) !! err) r
interpretStoreXa =
  interpretQStoreXa @Maybe

interpretQStores ::
  ∀ f err q d r .
  ResultShape d (f d) =>
  Members [Scoped ConnectionSource (DbTable d !! err), DbTable d !! err, Log, Embed IO] r =>
  TableSchema d ->
  QuerySchema q d ->
  InterpretersFor [QStore f q d !! err, Scoped Connection (QStore f q d !! err) !! err] r
interpretQStores table query =
  interpretQStoreXa table query .
  interpretQStoreDb table query

interpretStores ::
  ∀ err i d r .
  Members [Scoped ConnectionSource (DbTable (Uid i d) !! err), StoreTable i d !! err, Log, Embed IO] r =>
  TableSchema (Uid i d) ->
  QuerySchema i (Uid i d) ->
  InterpretersFor [Store i d !! err, Scoped Connection (Store i d !! err) !! err] r
interpretStores =
  interpretQStores @Maybe
