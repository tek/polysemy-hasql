module Polysemy.Hasql.Interpreter.StoreUpdateTree where

import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Db.Data.PartialField (PartialField)
import qualified Polysemy.Db.Effect.StoreUpdateTree as StoreUpdateTree
import Polysemy.Db.Effect.StoreUpdateTree (StoreUpdateTree)
import Polysemy.Db.Tree.Fold (FoldTree)

import qualified Polysemy.Hasql.Data.ManagedTable as ManagedTable
import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
import Polysemy.Hasql.Data.Query (Query)
import Polysemy.Hasql.Data.QueryTable (QueryTable)
import Polysemy.Hasql.ManagedTable (queryTable)
import qualified Polysemy.Hasql.Statement as Statement
import Polysemy.Hasql.Table.Query.Update (PartialSql)

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

interpretStoreUpdateTreeNull ::
  InterpreterFor (StoreUpdateTree i d paths !! e) r
interpretStoreUpdateTreeNull =
  interpretResumable \case
    StoreUpdateTree.Partial _ _ ->
      pure Nothing
{-# inline interpretStoreUpdateTreeNull #-}
