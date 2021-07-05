module Polysemy.Hasql.Interpreter.StoreUpdate where

import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Db.Data.PartialUpdate (PartialUpdate (PartialUpdate))
import Polysemy.Db.Data.Rep (UidRep)
import qualified Polysemy.Db.Effect.StoreUpdate as StoreUpdate
import Polysemy.Db.Effect.StoreUpdate (StoreUpdate)
import Polysemy.Db.Tree.Partial (Partial (Partial))
import Polysemy.Db.Tree.Partial.Insert (InsertPaths)

import qualified Polysemy.Hasql.Data.ManagedTable as ManagedTable
import Polysemy.Hasql.Data.ManagedTable (ManagedTableUid)
import Polysemy.Hasql.Data.Query (UidQuery)
import Polysemy.Hasql.Data.QueryTable (UidQueryTable)
import Polysemy.Hasql.ManagedTable (queryTable)
import Polysemy.Hasql.Query (interpretQuery)
import qualified Polysemy.Hasql.Statement as Statement
import Polysemy.Hasql.Store (StoreDeps, StoreStack, interpretStoreDbFullGenAs)
import Polysemy.Hasql.Table.Query.Update (BuildPartialSql)
import Polysemy.Hasql.Table.Schema (UidQuerySchema)

interpretStoreUpdateDbWith ::
  ∀ i d paths e tree r .
  InsertPaths d paths tree =>
  BuildPartialSql d tree =>
  Member (ManagedTableUid i d !! e) r =>
  UidQueryTable i d ->
  InterpreterFor (StoreUpdate i d paths !! e) r
interpretStoreUpdateDbWith table =
  interpretResumable \case
    StoreUpdate.Create i (PartialUpdate t) ->
      restop (ManagedTable.runStatement () (Statement.update table i t))
    StoreUpdate.Use i (Partial t) ->
      restop (ManagedTable.runStatement () (Statement.update table i t))

interpretStoreUpdateDb ::
  ∀ i d paths e tree r .
  Show e =>
  InsertPaths d paths tree =>
  BuildPartialSql d tree =>
  Members [UidQuery i d, ManagedTableUid i d !! e, Error InitDbError] r =>
  InterpreterFor (StoreUpdate i d paths !! e) r
interpretStoreUpdateDb sem = do
  table <- queryTable
  interpretStoreUpdateDbWith @_ @_ @_ @_ @tree table sem
{-# inline interpretStoreUpdateDb #-}

interpretStoreUpdateDbFull ::
  ∀ qrep irep rep i d paths tree t dt r .
  UidQuerySchema qrep irep rep i i d =>
  InsertPaths d paths tree =>
  BuildPartialSql d tree =>
  Members (Error InitDbError : StoreDeps t dt) r =>
  InterpretersFor (StoreUpdate i d paths !! DbError : StoreStack i d) r
interpretStoreUpdateDbFull =
  interpretStoreDbFullGenAs @qrep @irep @rep @i @d .
  interpretQuery @qrep @(UidRep irep rep) .
  interpretStoreUpdateDb @i @d .
  raiseUnder
{-# inline interpretStoreUpdateDbFull #-}
