module Polysemy.Hasql.Interpreter.StoreUpdate where

import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Db.Data.PartialField (PartialField)
import qualified Polysemy.Db.Effect.StoreUpdate as StoreUpdate
import Polysemy.Db.Effect.StoreUpdate (StoreUpdate, UidStoreUpdate)
import Polysemy.Db.Tree.Fold (FoldTree)
import Polysemy.Db.Tree.Partial (Partial (Partial))
import Polysemy.Db.Tree.Partial.Insert (InsertPaths, PartialUpdate (PartialUpdate))

import qualified Polysemy.Hasql.Data.ManagedTable as ManagedTable
import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
import Polysemy.Hasql.Data.Query (Query)
import Polysemy.Hasql.Data.QueryTable (QueryTable)
import Polysemy.Hasql.ManagedTable (queryTable)
import Polysemy.Hasql.Query (interpretQuery)
import qualified Polysemy.Hasql.Statement as Statement
import Polysemy.Hasql.Store (StoreDeps, interpretStoreDbFullGen)
import Polysemy.Hasql.Table.Query.Update (PartialSql)
import Polysemy.Hasql.Table.Schema (Schema)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.Rep (UidRep, PrimQuery)
import Polysemy.Db.Data.Uid (Uid)

interpretStoreUpdateDbWith ::
  ∀ i d paths e tree r .
  InsertPaths d paths tree =>
  FoldTree 'True () PartialField [PartialSql] tree =>
  Member (ManagedTable d !! e) r =>
  QueryTable i d ->
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
  FoldTree 'True () PartialField [PartialSql] tree =>
  Members [Query i d, ManagedTable d !! e, Error InitDbError] r =>
  InterpreterFor (StoreUpdate i d paths !! e) r
interpretStoreUpdateDb sem = do
  table <- queryTable
  interpretStoreUpdateDbWith @_ @_ @_ @_ @tree table sem
{-# inline interpretStoreUpdateDb #-}

interpretStoreUpdateDbFull ::
  ∀ qrep rep q d paths tree t dt r .
  Schema qrep rep q d =>
  InsertPaths d paths tree =>
  FoldTree 'True () PartialField [PartialSql] tree =>
  Members (Error InitDbError : StoreDeps t dt) r =>
  InterpreterFor (StoreUpdate q d paths !! DbError) r
interpretStoreUpdateDbFull =
  interpretStoreDbFullGen @qrep @rep @q @d .
  interpretQuery @qrep @rep .
  interpretStoreUpdateDb @q @d .
  raiseUnder3 .
  raiseUnder
{-# inline interpretStoreUpdateDbFull #-}

interpretStoreUpdateDbFullUidAs ::
  ∀ qrep rep ir i q d paths tree t dt r .
  Schema qrep (UidRep ir rep) q (Uid i d) =>
  InsertPaths (Uid i d) paths tree =>
  FoldTree 'True () PartialField [PartialSql] tree =>
  Members (Error InitDbError : StoreDeps t dt) r =>
  InterpreterFor (StoreUpdate q (Uid i d) paths !! DbError) r
interpretStoreUpdateDbFullUidAs =
  interpretStoreUpdateDbFull @qrep @(UidRep ir rep)
{-# inline interpretStoreUpdateDbFullUidAs #-}

interpretStoreUpdateDbFullUid ::
  ∀ rep ir i d paths tree t dt r .
  Schema (PrimQuery "id") (UidRep ir rep) i (Uid i d) =>
  InsertPaths (Uid i d) paths tree =>
  FoldTree 'True () PartialField [PartialSql] tree =>
  Members (Error InitDbError : StoreDeps t dt) r =>
  InterpreterFor (UidStoreUpdate i d paths !! DbError) r
interpretStoreUpdateDbFullUid =
  interpretStoreUpdateDbFullUidAs @(PrimQuery "id") @rep @ir
{-# inline interpretStoreUpdateDbFullUid #-}
