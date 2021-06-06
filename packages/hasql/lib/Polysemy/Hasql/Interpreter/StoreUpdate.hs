module Polysemy.Hasql.Interpreter.StoreUpdate where

import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Db.Data.PartialField (PartialField)
import qualified Polysemy.Db.Effect.StoreUpdate as StoreUpdate
import Polysemy.Db.Effect.StoreUpdate (StoreUpdate)
import Polysemy.Db.Tree.Fold (FoldTree)
import Polysemy.Db.Tree.Partial.Insert (InsertPaths, PartialUpdate (PartialUpdate))

import qualified Polysemy.Hasql.Data.ManagedTable as ManagedTable
import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
import Polysemy.Hasql.Data.Query (Query)
import Polysemy.Hasql.Data.QueryTable (QueryTable)
import Polysemy.Hasql.ManagedTable (queryTable)
import qualified Polysemy.Hasql.Statement as Statement
import Polysemy.Hasql.Table.Query.Update (PartialSql)

interpretStoreUpdateDbWith ::
  ∀ i d paths e tree r .
  InsertPaths d paths tree =>
  FoldTree 'True () PartialField [PartialSql] tree =>
  Member (ManagedTable d !! e) r =>
  QueryTable i d ->
  InterpreterFor (StoreUpdate i d paths !! e) r
interpretStoreUpdateDbWith table =
  interpretResumable \case
    StoreUpdate.Partial i (PartialUpdate t) ->
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

interpretStoreUpdateNull ::
  InterpreterFor (StoreUpdate i d paths !! e) r
interpretStoreUpdateNull =
  interpretResumable \case
    StoreUpdate.Partial _ _ ->
      pure Nothing
{-# inline interpretStoreUpdateNull #-}
