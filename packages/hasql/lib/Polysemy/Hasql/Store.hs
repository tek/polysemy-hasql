module Polysemy.Hasql.Store where

import Hasql.Connection (Connection)

import Polysemy.Db.Data.Column (UidRep)
import Polysemy.Db.Data.DbConfig (DbConfig)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.IdQuery (IdQuery(IdQuery))
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store, UidStore)
import qualified Polysemy.Db.Data.StoreError as StoreError
import Polysemy.Db.Data.StoreError (StoreError)
import Polysemy.Db.Data.Uid (Uid)
import Polysemy.Hasql.Data.Database (Database)
import Polysemy.Hasql.Data.DbConnection (DbConnection)
import Polysemy.Hasql.Data.QueryTable (QueryTable(QueryTable))
import Polysemy.Hasql.Data.Schema (Schema(..))
import Polysemy.Hasql.Data.Table (Table(Table))
import Polysemy.Hasql.Database (interpretDatabase)
import Polysemy.Hasql.DbConnection (interpretDbConnection)
import Polysemy.Hasql.Schema.Generic (interpretSchema)
import Polysemy.Hasql.Store.Statement (delete, fetch, fetchAll, insert, upsert)
import Polysemy.Hasql.Table.QueryTable (GenQueryTable, genQueryTable)
import Polysemy.Resource (Resource)

type StoreStack qOut dOut qIn dIn =
  [Store qOut DbError dOut, Schema qIn dIn, Database DbError dIn]

type UidStoreStack i d =
  StoreStack i (Uid i d) (IdQuery i) (Uid i d)

interpretStoreDb ::
  Members [Schema q d, Database DbError d] r =>
  InterpreterFor (Store q DbError d) r
interpretStoreDb =
  interpret \case
    Store.Insert record ->
      insert record
    Store.Upsert record ->
      upsert record
    Store.Delete id' ->
      delete id'
    Store.Fetch id' ->
      fetch id'
    Store.FetchAll ->
      second nonEmpty <$> fetchAll

interpretStoreDbAs' ::
  Member (Store qIn DbError dIn) r =>
  (dIn -> dOut) ->
  (dOut -> dIn) ->
  (qOut -> qIn) ->
  InterpreterFor (Store qOut DbError dOut) r
interpretStoreDbAs' toD fromD fromQ =
  interpret \case
    Store.Insert record ->
      Store.insert (fromD record)
    Store.Upsert record ->
      Store.upsert (fromD record)
    Store.Delete id' ->
      Store.delete (fromQ id')
    Store.Fetch id' ->
      (fmap . fmap) toD <$> Store.fetch (fromQ id')
    Store.FetchAll ->
      (fmap . fmap . fmap) toD <$> Store.fetchAll

interpretStoreDbAs ::
  Members [Schema qIn dIn, Database DbError dIn] r =>
  (dIn -> dOut) ->
  (dOut -> dIn) ->
  (qOut -> qIn) ->
  InterpreterFor (Store qOut DbError dOut) r
interpretStoreDbAs toD fromD fromQ =
  interpretStoreDb . interpretStoreDbAs' toD fromD fromQ . raiseUnder

dbConnectionError ::
  DbError ->
  InterpreterFor (Store q DbError d) r
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
      storeError :: Sem r (Either (StoreError DbError) a)
      storeError =
        pure (Left (StoreError.Backend err))

type StoreDeps =
  [DbConnection Connection, Embed IO]

interpretStoreDbFullAs ::
  Members StoreDeps r =>
  (dIn -> dOut) ->
  (dOut -> dIn) ->
  (qOut -> qIn) ->
  QueryTable qIn dIn ->
  InterpretersFor [Store qOut DbError dOut, Schema qIn dIn, Database DbError dIn] r
interpretStoreDbFullAs toD fromD fromQ qTable@(QueryTable (Table structure _ _) _ _) =
  interpretDatabase structure .
  interpretSchema qTable .
  interpretStoreDbAs toD fromD fromQ

interpretStoreDbFullUid ::
  ∀ i d r .
  Members StoreDeps r =>
  QueryTable (IdQuery i) (Uid i d) ->
  InterpretersFor [UidStore i DbError d, Schema (IdQuery i) (Uid i d), Database DbError (Uid i d)] r
interpretStoreDbFullUid =
  interpretStoreDbFullAs id id IdQuery

interpretStoreDbFullGenAs ::
  ∀ rep dIn dOut qIn qOut r .
  GenQueryTable rep qIn dIn =>
  Members StoreDeps r =>
  (dIn -> dOut) ->
  (dOut -> dIn) ->
  (qOut -> qIn) ->
  InterpretersFor [Store qOut DbError dOut, Schema qIn dIn, Database DbError dIn] r
interpretStoreDbFullGenAs toD fromD fromQ =
  interpretStoreDbFullAs toD fromD fromQ (genQueryTable @rep)

interpretStoreDbFullGenUid ::
  ∀ rep ir i d r .
  GenQueryTable (UidRep ir rep) (IdQuery i) (Uid i d) =>
  Members StoreDeps r =>
  InterpretersFor [UidStore i DbError d, Schema (IdQuery i) (Uid i d), Database DbError (Uid i d)] r
interpretStoreDbFullGenUid =
  interpretStoreDbFullGenAs @(UidRep ir rep) @(Uid i d) id id IdQuery

interpretStoreDbFull ::
  Members StoreDeps r =>
  QueryTable q d ->
  Sem (Store q DbError d : Schema q d : Database DbError d : r) a ->
  Sem r a
interpretStoreDbFull qTable@(QueryTable (Table structure _ _) _ _) =
  interpretDatabase structure .
  interpretSchema qTable .
  interpretStoreDb

interpretStoreDbFullGen ::
  ∀ rep q d r .
  GenQueryTable rep q d =>
  Members StoreDeps r =>
  InterpretersFor [Store q DbError d, Schema q d, Database DbError d] r
interpretStoreDbFullGen =
  interpretStoreDbFull (genQueryTable @rep)

interpretStoreDbSingle ::
  ∀ rep q d r .
  GenQueryTable rep q d =>
  Members [Error DbError, Resource, Embed IO] r =>
  DbConfig ->
  InterpretersFor [Store q DbError d, Schema q d, Database DbError d, DbConnection Connection] r
interpretStoreDbSingle host =
  interpretDbConnection host .
  interpretStoreDbFullGen @rep
