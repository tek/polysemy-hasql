module Polysemy.Hasql.Interpreter.StoreUpdateTree where

import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Db.Data.PartialField (PartialField)
import qualified Polysemy.Db.Effect.StoreUpdateTree as StoreUpdateTree
import Polysemy.Db.Effect.StoreUpdateTree (StoreUpdateTree, UidStoreUpdateTree)
import Polysemy.Db.Tree.Fold (FoldTree)

import qualified Polysemy.Hasql.Data.ManagedTable as ManagedTable
import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
import Polysemy.Hasql.Data.Query (Query)
import Polysemy.Hasql.Data.QueryTable (QueryTable)
import Polysemy.Hasql.ManagedTable (queryTable)
import qualified Polysemy.Hasql.Statement as Statement
import Polysemy.Hasql.Table.Query.Update (PartialSql)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Hasql.Store (StoreDeps, interpretStoreDbFullGen, StoreStack, UidStoreStack', UidStoreStack)
import Polysemy.Hasql.Table.Schema (Schema)
import Polysemy.Hasql.Query (interpretQuery)
import Polysemy.Db.Data.Uid (Uid)
import Polysemy.Db.Data.Rep (UidRep, PrimQuery)

interpretStoreUpdateTreeDbWith ::
  ∀ i d e tree r .
  FoldTree 'True () PartialField [PartialSql] tree =>
  Member (ManagedTable d !! e) r =>
  QueryTable i d ->
  InterpreterFor (StoreUpdateTree i d tree !! e) r
interpretStoreUpdateTreeDbWith table =
  interpretResumable \case
    StoreUpdateTree.Partial i t ->
      restop (ManagedTable.runStatement () (Statement.update table i t))
{-# inline interpretStoreUpdateTreeDbWith #-}

interpretStoreUpdateTreeDb ::
  ∀ i d e tree r .
  Show e =>
  FoldTree 'True () PartialField [PartialSql] tree =>
  Members [Query i d, ManagedTable d !! e, Error InitDbError] r =>
  InterpreterFor (StoreUpdateTree i d tree !! e) r
interpretStoreUpdateTreeDb sem = do
  table <- queryTable
  interpretStoreUpdateTreeDbWith @_ @_ @_ @tree table sem
{-# inline interpretStoreUpdateTreeDb #-}

interpretStoreUpdateTreeDbFull ::
  ∀ qrep rep q d tree t dt r .
  Schema qrep rep q d =>
  FoldTree 'True () PartialField [PartialSql] tree =>
  Members (Error InitDbError : StoreDeps t dt) r =>
  InterpretersFor (StoreUpdateTree q d tree !! DbError : StoreStack q d) r
interpretStoreUpdateTreeDbFull =
  interpretStoreDbFullGen @qrep @rep @q @d .
  interpretQuery @qrep @rep .
  interpretStoreUpdateTreeDb @q @d .
  raiseUnder
{-# inline interpretStoreUpdateTreeDbFull #-}

interpretStoreUpdateTreeDbFullUidAs ::
  ∀ qrep rep ir i q d tree t dt r .
  Schema qrep (UidRep ir rep) q (Uid i d) =>
  FoldTree 'True () PartialField [PartialSql] tree =>
  Members (Error InitDbError : StoreDeps t dt) r =>
  InterpretersFor (StoreUpdateTree q (Uid i d) tree !! DbError : UidStoreStack' i q d) r
interpretStoreUpdateTreeDbFullUidAs =
  interpretStoreUpdateTreeDbFull @qrep @(UidRep ir rep)
{-# inline interpretStoreUpdateTreeDbFullUidAs #-}

interpretStoreUpdateTreeDbFullUid ::
  ∀ rep ir i d tree t dt r .
  Schema (PrimQuery "id") (UidRep ir rep) i (Uid i d) =>
  FoldTree 'True () PartialField [PartialSql] tree =>
  Members (Error InitDbError : StoreDeps t dt) r =>
  InterpretersFor (UidStoreUpdateTree i d tree !! DbError : UidStoreStack i d) r
interpretStoreUpdateTreeDbFullUid =
  interpretStoreUpdateTreeDbFullUidAs @(PrimQuery "id") @rep @ir
{-# inline interpretStoreUpdateTreeDbFullUid #-}
