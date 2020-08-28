module Polysemy.Db.Store where

import Hasql.Connection (Connection)

import Polysemy.Db.Data.Column (PK, PKQuery(PKQuery), PKRep, pkToUid, uidToPK)
import Polysemy.Db.Data.Database (Database)
import Polysemy.Db.Data.DbConfig (DbConfig)
import Polysemy.Db.Data.DbConnection (DbConnection)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.QueryTable (QueryTable(QueryTable))
import Polysemy.Db.Data.Schema (Schema(..))
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)
import qualified Polysemy.Db.Data.StoreError as StoreError
import Polysemy.Db.Data.StoreError (StoreError)
import Polysemy.Db.Data.Table (Table(Table))
import Polysemy.Db.Data.Uid (Uid)
import Polysemy.Db.Database (interpretDatabase)
import Polysemy.Db.DbConnection (interpretDbConnection)
import Polysemy.Db.Schema.Generic (interpretSchema)
import Polysemy.Db.Store.Statement (delete, fetch, fetchAll, insert, upsert)
import Polysemy.Db.Table.QueryTable (GenQueryTable, genQueryTable)

interpretStoreDb ::
  Members [Schema q d, Database d DbError] r =>
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
  Members [Schema qIn dIn, Database dIn DbError] r =>
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
  QueryTable dIn qIn ->
  InterpreterFor (Store qOut DbError dOut) r
interpretStoreDbFullAs toD fromD fromQ qTable@(QueryTable (Table structure _ _) _ _) =
  interpretDatabase structure .
  interpretSchema qTable .
  interpretStoreDbAs toD fromD fromQ .
  raiseUnder2

interpretStoreDbFullGenAs ::
  ∀ rep dIn dOut qIn qOut r .
  GenQueryTable dIn rep qIn =>
  Members StoreDeps r =>
  (dIn -> dOut) ->
  (dOut -> dIn) ->
  (qOut -> qIn) ->
  InterpreterFor (Store qOut DbError dOut) r
interpretStoreDbFullGenAs toD fromD fromQ =
  interpretStoreDbFullAs toD fromD fromQ (genQueryTable @_ @rep)

interpretStoreDbFullGenUid ::
  ∀ i rep d r .
  GenQueryTable (PK i d) (PKRep i rep) (PKQuery i) =>
  Members StoreDeps r =>
  InterpreterFor (Store i DbError (Uid i d)) r
interpretStoreDbFullGenUid =
  interpretStoreDbFullGenAs @(PKRep i rep) pkToUid uidToPK PKQuery

interpretStoreDbFull ::
  Members StoreDeps r =>
  QueryTable d q ->
  InterpreterFor (Store q DbError d) r
interpretStoreDbFull qTable@(QueryTable (Table structure _ _) _ _) =
  interpretDatabase structure .
  interpretSchema qTable .
  interpretStoreDb .
  raiseUnder2

interpretStoreDbFullGen ::
  ∀ d rep q r .
  GenQueryTable d rep q =>
  Members StoreDeps r =>
  InterpreterFor (Store q DbError d) r
interpretStoreDbFullGen =
  interpretStoreDbFull (genQueryTable @_ @rep)

interpretStoreDbSingle ::
  ∀ d rep q r .
  GenQueryTable d rep q =>
  Members [Error DbError, Embed IO] r =>
  DbConfig ->
  InterpreterFor (Store q DbError d) r
interpretStoreDbSingle host =
  interpretDbConnection host .
  interpretStoreDbFullGen @_ @rep .
  raiseUnder
