module Polysemy.Hasql.Store where

import Hasql.Connection (Connection)

import Polysemy.Db.Data.Column (PK, PKQuery(PKQuery), PKRep, pkToUid, uidToPK)
import Polysemy.Db.Data.DbConfig (DbConfig)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)
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
  InterpreterFor (Store qOut DbError dOut) r
interpretStoreDbFullAs toD fromD fromQ qTable@(QueryTable (Table structure _ _) _ _) =
  interpretDatabase structure .
  interpretSchema qTable .
  interpretStoreDbAs toD fromD fromQ .
  raiseUnder2

interpretStoreDbFullGenAs ::
  ∀ rep dIn dOut qIn qOut r .
  GenQueryTable rep qIn dIn =>
  Members StoreDeps r =>
  (dIn -> dOut) ->
  (dOut -> dIn) ->
  (qOut -> qIn) ->
  InterpreterFor (Store qOut DbError dOut) r
interpretStoreDbFullGenAs toD fromD fromQ =
  interpretStoreDbFullAs toD fromD fromQ (genQueryTable @rep)

interpretStoreDbFullGenUid ::
  ∀ i rep d f r .
  GenQueryTable (PKRep f i rep) (PKQuery i) (PK f i d) =>
  Members StoreDeps r =>
  InterpreterFor (Store i DbError (Uid i d)) r
interpretStoreDbFullGenUid =
  interpretStoreDbFullGenAs @(PKRep f i rep) @(PK f i d) pkToUid uidToPK PKQuery

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
  ∀ rep q d r a .
  GenQueryTable rep q d =>
  Members StoreDeps r =>
  Sem (Store q DbError d : Schema q d : Database DbError d : r) a ->
  Sem r a
interpretStoreDbFullGen =
  interpretStoreDbFull (genQueryTable @rep)

interpretStoreDbSingle ::
  ∀ rep q d r a .
  GenQueryTable rep q d =>
  Members [Error DbError, Embed IO] r =>
  DbConfig ->
  Sem (Store q DbError d : Schema q d : Database DbError d : DbConnection Connection : r) a ->
  Sem r a
interpretStoreDbSingle host =
  interpretDbConnection host .
  interpretStoreDbFullGen @rep
