module Polysemy.Hasql.Store where

import Polysemy (raise3Under)
import Polysemy.Db.Data.Column (PrimQuery, UidRep)
import Polysemy.Db.Data.DbConfig (DbConfig)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)
import Polysemy.Db.Data.Uid (Uid)
import Polysemy.Log (Log)
import Polysemy.Resource (Resource)
import Polysemy.Time (Time, interpretTimeGhc)

import Polysemy.Hasql.Crud (interpretCrudWith)
import Polysemy.Hasql.Data.Crud (Crud(..))
import Polysemy.Hasql.Data.Database (Database)
import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
import qualified Polysemy.Hasql.Data.QueryTable as QueryTable
import Polysemy.Hasql.Data.QueryTable (QueryTable)
import Polysemy.Hasql.Database (interpretDatabase)
import Polysemy.Hasql.DbConnection (interpretDbConnection)
import Polysemy.Hasql.ManagedTable (interpretManagedTable)
import qualified Polysemy.Hasql.Store.Statement as Statement
import Polysemy.Hasql.Table.QueryTable (GenQueryTable, genQueryTable)

type StoreStack qOut dOut qIn dIn =
  [Store qOut dOut !! DbError, Crud qIn dIn !! DbError, ManagedTable dIn !! DbError]

type UidStoreStack' i q d =
  [Store q (Uid i d) !! DbError, Crud q (Uid i d) !! DbError, ManagedTable (Uid i d) !! DbError]

type UidStoreStack i d =
  StoreStack i (Uid i d) i (Uid i d)

interpretStoreDb ::
  Members [Crud q d !! e, ManagedTable d !! e] r =>
  InterpreterFor (Store q d !! e) r
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
    Store.Update id' values ->
      Statement.update id' values

interpretStoreDbUid ::
  Members [Crud i (Uid i d) !! e, ManagedTable (Uid i d) !! e] r =>
  InterpreterFor (Store i (Uid i d) !! e) r
interpretStoreDbUid =
  interpretStoreDb

type StoreDeps t dt =
  [Database !! DbError, Time t dt, Log, Embed IO]

interpretStoreDbFullGenUidAs ::
  ∀ qrep rep ir i q d t dt r .
  GenQueryTable qrep (UidRep ir rep) q (Uid i d) =>
  Members (StoreDeps t dt) r =>
  InterpretersFor (UidStoreStack' i q d) r
interpretStoreDbFullGenUidAs =
  interpretStoreDbFullGen @qrep @(UidRep ir rep) @q @(Uid i d)

interpretStoreDbFullGenUid ::
  ∀ rep ir i d t dt r .
  GenQueryTable (PrimQuery "id") (UidRep ir rep) i (Uid i d) =>
  Members (StoreDeps t dt) r =>
  InterpretersFor (UidStoreStack i d) r
interpretStoreDbFullGenUid =
  interpretStoreDbFullGenUidAs @(PrimQuery "id") @rep @ir

interpretStoreDbFull ::
  Members (StoreDeps t dt) r =>
  QueryTable q d ->
  InterpretersFor (StoreStack q d q d) r
interpretStoreDbFull table =
  interpretManagedTable (table ^. QueryTable.table) .
  interpretCrudWith table .
  interpretStoreDb

interpretStoreDbFullUid ::
  ∀ i q d t dt r .
  Members (StoreDeps t dt) r =>
  QueryTable q (Uid i d) ->
  InterpretersFor (UidStoreStack' i q d) r
interpretStoreDbFullUid =
  interpretStoreDbFull

-- |Out-of-the box interpreter for 'Store' that generically derives a table and delegates queries to 'Crud' and table
-- housekeeping to 'ManagedTable'.
--
-- @
-- runM $ resourceToIO $ interpretTimeGhc $ interpretDbConnection $ interpretDatabase $ interpretStoreDbFullGen @Auto do
--   Store.insert (User 1 "admin")
--   Store.fetchAll
-- @
interpretStoreDbFullGen ::
  ∀ qrep rep q d t dt r .
  GenQueryTable qrep rep q d =>
  Members (StoreDeps t dt) r =>
  InterpretersFor (StoreStack q d q d) r
interpretStoreDbFullGen =
  interpretStoreDbFull (genQueryTable @qrep @rep)

interpretStoreDbSingle ::
  ∀ qrep rep q d r .
  GenQueryTable qrep rep q d =>
  Members [Resource, Log, Embed IO, Final IO] r =>
  Text ->
  DbConfig ->
  InterpretersFor (StoreStack q d q d) r
interpretStoreDbSingle name host =
  interpretDbConnection name host .
  interpretTimeGhc .
  interpretDatabase .
  interpretStoreDbFullGen @qrep @rep .
  raise3Under .
  raise3Under .
  raise3Under
