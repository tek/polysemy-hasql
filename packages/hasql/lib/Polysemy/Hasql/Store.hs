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
import Polysemy.Hasql (HasqlConnection)
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

type StoreStack qOut dOut qIn dIn =
  [Store qOut dOut ! DbError, Schema qIn dIn ! DbError, ManagedTable dIn ! DbError]

type UidStoreStack i d =
  StoreStack i (Uid i d) (IdQuery i) (Uid i d)

interpretStoreDb ::
  Members [Schema q d ! DbError, ManagedTable d ! DbError] r =>
  InterpreterFor (Store q d ! DbError) r
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
  Member (Store qIn dIn ! DbError) r =>
  (dIn -> dOut) ->
  (dOut -> dIn) ->
  (qOut -> qIn) ->
  InterpreterFor (Store qOut dOut ! DbError) r
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
  Members [Schema qIn dIn ! DbError, ManagedTable dIn ! DbError] r =>
  (dIn -> dOut) ->
  (dOut -> dIn) ->
  (qOut -> qIn) ->
  InterpreterFor (Store qOut dOut ! DbError) r
interpretStoreDbAs toD fromD fromQ =
  interpretStoreDb . interpretStoreDbAs' toD fromD fromQ . raiseUnder

dbConnectionError ::
  ∀ q d r .
  Member (Stop DbError) r =>
  DbError ->
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

type StoreDeps t dt =
  [HasqlConnection, Time t dt, Embed IO]

interpretStoreDbFullAs ::
  Members (StoreDeps t dt) r =>
  (dIn -> dOut) ->
  (dOut -> dIn) ->
  (qOut -> qIn) ->
  QueryTable qIn dIn ->
  InterpretersFor (StoreStack qOut dOut qIn dIn) r
interpretStoreDbFullAs toD fromD fromQ table =
  interpretDatabase .
  interpretManagedTable (table ^. structure) .
  raiseUnder .
  resumable (interpretSchema table) .
  interpretStoreDbAs toD fromD fromQ

interpretStoreDbFullUid ::
  ∀ i d t dt r .
  Members (StoreDeps t dt) r =>
  QueryTable (IdQuery i) (Uid i d) ->
  InterpretersFor (UidStoreStack i d) r
interpretStoreDbFullUid =
  interpretStoreDbFullAs id id IdQuery

interpretStoreDbFullGenAs ::
  ∀ rep dIn dOut qIn qOut t dt r .
  GenQueryTable rep qIn dIn =>
  Members (StoreDeps t dt) r =>
  (dIn -> dOut) ->
  (dOut -> dIn) ->
  (qOut -> qIn) ->
  InterpretersFor (StoreStack qOut dOut qIn dIn) r
interpretStoreDbFullGenAs toD fromD fromQ =
  interpretStoreDbFullAs toD fromD fromQ (genQueryTable @rep)

interpretStoreDbFullGenUid ::
  ∀ rep ir i d t dt r .
  GenQueryTable (UidRep ir rep) (IdQuery i) (Uid i d) =>
  Members (StoreDeps t dt) r =>
  InterpretersFor (UidStoreStack i d) r
interpretStoreDbFullGenUid =
  interpretStoreDbFullGenAs @(UidRep ir rep) @(Uid i d) id id IdQuery

interpretStoreDbFull ::
  Members (StoreDeps t dt) r =>
  QueryTable q d ->
  InterpretersFor (StoreStack q d q d) r
interpretStoreDbFull table =
  interpretDatabase .
  interpretManagedTable (table ^. structure) .
  raiseUnder .
  resumable (interpretSchema table) .
  interpretStoreDb

interpretStoreDbFullGen ::
  ∀ rep q d t dt r .
  GenQueryTable rep q d =>
  Members (StoreDeps t dt) r =>
  InterpretersFor [Store q d ! DbError, Schema q d ! DbError, ManagedTable d ! DbError] r
interpretStoreDbFullGen =
  interpretStoreDbFull (genQueryTable @rep)

interpretStoreDbSingle ::
  ∀ rep q d r .
  GenQueryTable rep q d =>
  Members [Resource, Embed IO] r =>
  DbConfig ->
  InterpretersFor (StoreStack q d q d) r
interpretStoreDbSingle host =
  interpretDbConnection host .
  interpretTimeGhc .
  interpretStoreDbFullGen @rep .
  raise3Under .
  raise3Under
