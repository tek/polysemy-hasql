module Polysemy.Hasql.Store where

import Polysemy.Resource (Resource)
import Polysemy.Resume (Stop, stop, type (!))
import Polysemy.Time (Time, interpretTimeGhc)

import Polysemy (raise3Under)
import Polysemy.Db.Data.Column (UidRep)
import Polysemy.Db.Data.DbConfig (DbConfig)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.IdQuery (IdQuery(IdQuery))
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)
import Polysemy.Db.Data.Uid (Uid)
import Polysemy.Hasql.Data.Database (Database)
import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
import Polysemy.Hasql.Data.QueryTable (QueryTable, structure)
import Polysemy.Hasql.Data.Schema (Schema(..))
import Polysemy.Hasql.Database (interpretDatabase)
import Polysemy.Hasql.DbConnection (interpretDbConnection)
import Polysemy.Hasql.ManagedTable (interpretManagedTable)
import Polysemy.Hasql.Schema.Generic (interpretSchema)
import Polysemy.Hasql.Store.Statement (delete, fetch, fetchAll, insert, upsert)
import Polysemy.Hasql.Table.QueryTable (GenQueryTable, genQueryTable)
import Polysemy.Resume (interpretResumable, restop, resumable)

type StoreStack e qOut dOut qIn dIn =
  [Store qOut dOut ! e, Schema qIn dIn ! e, ManagedTable dIn ! e]

type UidStoreStack i e d =
  StoreStack e i (Uid i d) (IdQuery i) (Uid i d)

interpretStoreDb ::
  Members [Schema q d ! e, ManagedTable d ! e] r =>
  InterpreterFor (Store q d ! e) r
interpretStoreDb =
  interpretResumable \case
    Store.Insert record ->
      insert record
    Store.Upsert record ->
      upsert record
    Store.Delete id' ->
      delete id'
    Store.Fetch id' ->
      fetch id'
    Store.FetchAll ->
      nonEmpty <$> fetchAll

interpretStoreDbAs' ::
  Member (Store qIn dIn ! e) r =>
  (dIn -> dOut) ->
  (dOut -> dIn) ->
  (qOut -> qIn) ->
  InterpreterFor (Store qOut dOut ! e) r
interpretStoreDbAs' toD fromD fromQ =
  interpretResumable \case
    Store.Insert record ->
      restop (Store.insert (fromD record))
    Store.Upsert record ->
      restop (Store.upsert (fromD record))
    Store.Delete id' ->
      restop (Store.delete (fromQ id'))
    Store.Fetch id' ->
      restop (fmap toD <$> Store.fetch (fromQ id'))
    Store.FetchAll ->
      restop ((fmap . fmap) toD <$> Store.fetchAll)

interpretStoreDbAs ::
  Members [Schema qIn dIn ! e, ManagedTable dIn ! e] r =>
  (dIn -> dOut) ->
  (dOut -> dIn) ->
  (qOut -> qIn) ->
  InterpreterFor (Store qOut dOut ! e) r
interpretStoreDbAs toD fromD fromQ =
  interpretStoreDb . interpretStoreDbAs' toD fromD fromQ . raiseUnder

dbConnectionError ::
  ∀ e q d r .
  Member (Stop e) r =>
  e ->
  InterpreterFor (Store q d) r
dbConnectionError err =
  interpret \case
    Store.Insert _ ->
      storeError
    Store.Upsert _ ->
      storeError
    Store.Delete _ ->
      storeError
    Store.Fetch _ ->
      storeError
    Store.FetchAll ->
      storeError
    where
      storeError :: Sem r a
      storeError =
        stop err

type StoreDeps t dt e =
  [Database ! e, Time t dt, Embed IO]

interpretStoreDbFullAs ::
  Members (StoreDeps t dt e) r =>
  (dIn -> dOut) ->
  (dOut -> dIn) ->
  (qOut -> qIn) ->
  QueryTable qIn dIn ->
  InterpretersFor (StoreStack e qOut dOut qIn dIn) r
interpretStoreDbFullAs toD fromD fromQ table =
  interpretManagedTable (table ^. structure) .
  resumable (interpretSchema table) .
  interpretStoreDbAs toD fromD fromQ

interpretStoreDbFullUid ::
  ∀ i d e t dt r .
  Members (StoreDeps t dt e) r =>
  QueryTable (IdQuery i) (Uid i d) ->
  InterpretersFor (UidStoreStack i e d) r
interpretStoreDbFullUid =
  interpretStoreDbFullAs id id IdQuery

interpretStoreDbFullGenAs ::
  ∀ rep dIn dOut qIn qOut e t dt r .
  GenQueryTable rep qIn dIn =>
  Members (StoreDeps t dt e) r =>
  (dIn -> dOut) ->
  (dOut -> dIn) ->
  (qOut -> qIn) ->
  InterpretersFor (StoreStack e qOut dOut qIn dIn) r
interpretStoreDbFullGenAs toD fromD fromQ =
  interpretStoreDbFullAs toD fromD fromQ (genQueryTable @rep)

interpretStoreDbFullGenUid ::
  ∀ rep ir i d e t dt r .
  GenQueryTable (UidRep ir rep) (IdQuery i) (Uid i d) =>
  Members (StoreDeps t dt e) r =>
  InterpretersFor (UidStoreStack i e d) r
interpretStoreDbFullGenUid =
  interpretStoreDbFullGenAs @(UidRep ir rep) @(Uid i d) id id IdQuery

interpretStoreDbFull ::
  Members (StoreDeps t dt e) r =>
  QueryTable q d ->
  InterpretersFor (StoreStack e q d q d) r
interpretStoreDbFull table =
  interpretManagedTable (table ^. structure) .
  resumable (interpretSchema table) .
  interpretStoreDb

interpretStoreDbFullGen ::
  ∀ rep q d e t dt r .
  GenQueryTable rep q d =>
  Members (StoreDeps t dt e) r =>
  InterpretersFor (StoreStack e q d q d) r
interpretStoreDbFullGen =
  interpretStoreDbFull (genQueryTable @rep)

interpretStoreDbSingle ::
  ∀ rep q d r .
  GenQueryTable rep q d =>
  Members [Resource, Embed IO] r =>
  DbConfig ->
  InterpretersFor (StoreStack DbError q d q d) r
interpretStoreDbSingle host =
  interpretDbConnection host .
  interpretTimeGhc .
  interpretDatabase .
  interpretStoreDbFullGen @rep .
  raise3Under .
  raise3Under .
  raise3Under
