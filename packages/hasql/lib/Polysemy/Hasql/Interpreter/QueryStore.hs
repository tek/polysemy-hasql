module Polysemy.Hasql.Interpreter.QueryStore where

import Hasql.Encoders (Params)
import Polysemy (raise3Under)
import Polysemy.Db.Data.DbConfig (DbConfig)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.InitDbError (InitDbError)
import qualified Polysemy.Db.Data.QueryStore as QueryStore
import Polysemy.Db.Data.QueryStore (QueryStore)
import Polysemy.Log (Log)
import Polysemy.Resource (Resource)
import Polysemy.Tagged (Tagged, untag)
import Polysemy.Time (Time, interpretTimeGhc)

import Polysemy.Hasql.Crud (interpretCrud, interpretCrudWith)
import Polysemy.Hasql.Data.Crud (Crud (..))
import Polysemy.Hasql.Data.Database (Database)
import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
import Polysemy.Hasql.Data.Query (Query)
import qualified Polysemy.Hasql.Data.QueryTable as QueryTable
import Polysemy.Hasql.Data.QueryTable (QueryTable)
import qualified Polysemy.Hasql.Data.Where as Data
import Polysemy.Hasql.Database (interpretDatabase)
import Polysemy.Hasql.DbConnection (interpretDbConnection)
import Polysemy.Hasql.ManagedTable (interpretManagedTable, interpretManagedTableGen)
import Polysemy.Hasql.Query (interpretQuery)
import qualified Polysemy.Hasql.Store.Statement as Statement
import Polysemy.Hasql.Table.Query.Update (BuildPartialSql)
import Polysemy.Hasql.Table.Schema (Schema)

type QueryStoreStack i d q p =
  [QueryStore i d q p !! DbError, Crud i d q p !! DbError, ManagedTable d !! DbError]

interpretQueryStoreDb ::
  Members [Crud i d q p !! e, ManagedTable d !! e] r =>
  InterpreterFor (QueryStore i d q p !! e) r
interpretQueryStoreDb =
  interpretResumable \case
    QueryStore.Insert record ->
      Statement.insert record
    QueryStore.Upsert record ->
      Statement.upsert record
    QueryStore.Delete id' ->
      nonEmpty <$> Statement.delete id'
    QueryStore.DeleteAll ->
      nonEmpty <$> Statement.deleteAll
    QueryStore.Fetch id' ->
      Statement.fetch id'
    QueryStore.FetchAll ->
      nonEmpty <$> Statement.fetchAll
    QueryStore.Query q ->
      nonEmpty <$> Statement.fetchQ q
    QueryStore.Update i patch ->
      Statement.update i patch
    QueryStore.UpdateQuery q patch ->
      Statement.updateQ q patch

type QueryStoreDeps t dt =
  [Database !! DbError, Time t dt, Log, Embed IO]

interpretQueryStoreDbFullWith ::
  BuildPartialSql p tree u =>
  Members (QueryStoreDeps t dt) r =>
  QueryTable q d ->
  Params i ->
  Data.Where i d ->
  InterpretersFor (QueryStoreStack i d q p) r
interpretQueryStoreDbFullWith table iParams iWhere =
  interpretManagedTable (table ^. QueryTable.table) .
  interpretCrudWith table iParams iWhere .
  interpretQueryStoreDb
{-# inline interpretQueryStoreDbFullWith #-}

interpretQueryStoreDbQuery ::
  ∀ qrep rep i d q p t dt r tree u .
  Schema qrep rep q d =>
  BuildPartialSql p tree u =>
  Members [Tagged "id" (Query i d), Tagged "main" (Query q d), Error InitDbError] r =>
  Members (QueryStoreDeps t dt) r =>
  InterpretersFor (QueryStoreStack i d q p) r
interpretQueryStoreDbQuery =
  interpretManagedTableGen @rep .
  interpretCrud .
  interpretQueryStoreDb
{-# inline interpretQueryStoreDbQuery #-}

interpretQueryStoreDbFullGenAs ::
  ∀ qrep iqrep rep i d q p t dt r tree u .
  Schema qrep rep q d =>
  Schema iqrep rep i d =>
  BuildPartialSql p tree u =>
  Members (Error InitDbError : QueryStoreDeps t dt) r =>
  InterpretersFor (QueryStoreStack i d q p) r
interpretQueryStoreDbFullGenAs =
  interpretQuery @qrep @rep @q @d .
  untag @"main" .
  interpretQuery @iqrep @rep @i @d .
  untag @"id" .
  interpretQueryStoreDbQuery @qrep @rep @i @d @q @p .
  raise3Under .
  raise3Under

interpretQueryStoreDbSingle ::
  ∀ qrep iqrep rep i d q p r tree u .
  Schema qrep rep q d =>
  Schema iqrep rep i d =>
  BuildPartialSql p tree u =>
  Members [Resource, Log, Error InitDbError, Embed IO, Final IO] r =>
  Text ->
  DbConfig ->
  InterpretersFor (QueryStoreStack i d q p) r
interpretQueryStoreDbSingle name host =
  interpretDbConnection name host .
  interpretTimeGhc .
  interpretDatabase .
  interpretQueryStoreDbFullGenAs @qrep @iqrep @rep .
  raise3Under .
  raise3Under .
  raise3Under
