module Polysemy.Hasql.Store where

import Polysemy (raise3Under)
import Polysemy.Db.Data.DbConfig (DbConfig)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.Rep (PrimQuery, PrimaryKey, UidRep)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)
import Polysemy.Db.Data.Uid (Uid)
import Polysemy.Log (Log)
import Polysemy.Resource (Resource)
import Polysemy.Time (Time, interpretTimeGhc)

import Polysemy.Hasql.Crud (interpretCrudUidNoUpdateWith, interpretCrudUidWith)
import Polysemy.Hasql.Data.Crud (Crud (..))
import Polysemy.Hasql.Data.Database (Database)
import Polysemy.Hasql.Data.ManagedTable (ManagedTableUid)
import qualified Polysemy.Hasql.Data.QueryTable as QueryTable
import Polysemy.Hasql.Data.QueryTable (QueryTable)
import Polysemy.Hasql.Database (interpretDatabase)
import Polysemy.Hasql.DbConnection (interpretDbConnection)
import Polysemy.Hasql.ManagedTable (interpretManagedTable)
import qualified Polysemy.Hasql.Store.Statement as Statement
import Polysemy.Hasql.Table.Query.Update (BuildPartialSql)
import Polysemy.Hasql.Table.Schema (UidQuerySchema, UidSchema, schema)

type StoreStack i d =
  [Store i d !! DbError, Crud i (Uid i d) i d !! DbError, ManagedTableUid i d !! DbError]

interpretStoreDb ::
  Members [Crud i (Uid i d) i d !! e, ManagedTableUid i d !! e] r =>
  InterpreterFor (Store i d !! e) r
interpretStoreDb =
  interpretResumable \case
    Store.Insert record ->
      Statement.insert record
    Store.Upsert record ->
      Statement.upsert record
    Store.Delete id' ->
      nonEmpty <$> Statement.delete id'
    Store.DeleteAll ->
      nonEmpty <$> Statement.deleteAll
    Store.Fetch id' ->
      Statement.fetch id'
    Store.FetchAll ->
      nonEmpty <$> Statement.fetchAll
    Store.Update i patch ->
      Statement.update i patch

type StoreDeps t dt =
  [Database !! DbError, Time t dt, Log, Embed IO]

interpretStoreDbFull ::
  BuildPartialSql d tree =>
  Members (StoreDeps t dt) r =>
  QueryTable i (Uid i d) ->
  InterpretersFor (StoreStack i d) r
interpretStoreDbFull table =
  interpretManagedTable (table ^. QueryTable.table) .
  interpretCrudUidWith table (table ^. QueryTable.qparams) (table ^. QueryTable.qwhere) .
  interpretStoreDb

interpretStoreDbFullGenAs ::
  ∀ qrep irep rep i d t dt r tree .
  UidQuerySchema qrep irep rep i i d =>
  BuildPartialSql d tree =>
  Members (StoreDeps t dt) r =>
  InterpretersFor (StoreStack i d) r
interpretStoreDbFullGenAs =
  interpretStoreDbFull (schema @qrep @(UidRep irep rep))

-- |Out-of-the box interpreter for 'Store' that generically derives a table and delegates queries to 'Crud' and table
-- housekeeping to 'ManagedTable'.
--
-- @
-- runM $ resourceToIO $ interpretTimeGhc $ interpretDbConnection $ interpretDatabase $ interpretStoreDbFullGen @Auto do
--   Store.insert (User 1 "admin")
--   Store.fetchAll
-- @
interpretStoreDbFullGen ::
  ∀ rep i d t dt r tree .
  UidSchema rep i d =>
  BuildPartialSql d tree =>
  Members (StoreDeps t dt) r =>
  InterpretersFor (StoreStack i d) r
interpretStoreDbFullGen =
  interpretStoreDbFullGenAs @(PrimQuery "id") @PrimaryKey @rep

interpretStoreDbSingle ::
  ∀ qrep irep rep i d r tree .
  UidQuerySchema qrep irep rep i i d =>
  BuildPartialSql d tree =>
  Members [Resource, Log, Embed IO, Final IO] r =>
  Text ->
  DbConfig ->
  InterpretersFor (StoreStack i d) r
interpretStoreDbSingle name host =
  interpretDbConnection name host .
  interpretTimeGhc .
  interpretDatabase .
  interpretStoreDbFullGenAs @qrep @irep @rep .
  raise3Under .
  raise3Under .
  raise3Under

interpretStoreDbFullNoUpdate ::
  Members (StoreDeps t dt) r =>
  QueryTable i (Uid i d) ->
  InterpretersFor (StoreStack i d) r
interpretStoreDbFullNoUpdate table =
  interpretManagedTable (table ^. QueryTable.table) .
  interpretCrudUidNoUpdateWith table (table ^. QueryTable.qparams) (table ^. QueryTable.qwhere) .
  interpretStoreDb

interpretStoreDbFullNoUpdateGenAs ::
  ∀ qrep irep rep i d t dt r .
  UidQuerySchema qrep irep rep i i d =>
  Members (StoreDeps t dt) r =>
  InterpretersFor (StoreStack i d) r
interpretStoreDbFullNoUpdateGenAs =
  interpretStoreDbFullNoUpdate (schema @qrep @(UidRep irep rep))

-- |Out-of-the box interpreter for 'Store' that generically derives a table and delegates queries to 'Crud' and table
-- housekeeping to 'ManagedTable'.
--
-- @
-- runM $ resourceToIO $ interpretTimeGhc $ interpretDbConnection $ interpretDatabase $ interpretStoreDbFullGen @Auto do
--   Store.insert (User 1 "admin")
--   Store.fetchAll
-- @
interpretStoreDbFullNoUpdateGen ::
  ∀ rep i d t dt r .
  UidSchema rep i d =>
  Members (StoreDeps t dt) r =>
  InterpretersFor (StoreStack i d) r
interpretStoreDbFullNoUpdateGen =
  interpretStoreDbFullNoUpdateGenAs @(PrimQuery "id") @PrimaryKey @rep
