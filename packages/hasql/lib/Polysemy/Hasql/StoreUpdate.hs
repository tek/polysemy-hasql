module Polysemy.Hasql.StoreUpdate where

import Polysemy.Db.Data.InitDbError (InitDbError)
import Polysemy.Db.Data.PartialField (PartialField)
import qualified Polysemy.Db.Data.StoreUpdate as StoreUpdate
import Polysemy.Db.Data.StoreUpdate (StoreUpdate)
import Polysemy.Db.Tree.Fold (FoldTree)
import Polysemy.Db.Tree.Partial (Partial, partial)

import qualified Polysemy.Hasql.Data.ManagedTable as ManagedTable
import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
import Polysemy.Hasql.Data.Query (Query)
import Polysemy.Hasql.Data.QueryTable (QueryTable)
import Polysemy.Hasql.ManagedTable (queryTable)
import qualified Polysemy.Hasql.Statement as Statement
import Polysemy.Hasql.Table.Query.Update (PartialSql)

interpretStoreUpdateDbWith ::
  ∀ i d e tree r .
  Partial d tree =>
  FoldTree () PartialField [PartialSql] tree =>
  Member (ManagedTable d !! e) r =>
  QueryTable i d ->
  InterpreterFor (StoreUpdate i d tree !! e) r
interpretStoreUpdateDbWith table =
  interpretResumable \case
    StoreUpdate.Update i f ->
      restop (ManagedTable.runStatement () (Statement.update table i (f ptree)))
  where
    ptree =
      partial @d

interpretStoreUpdateDb ::
  ∀ i d e tree r .
  Show e =>
  Partial d tree =>
  FoldTree () PartialField [PartialSql] tree =>
  Members [Query i d, ManagedTable d !! e, Error InitDbError] r =>
  InterpreterFor (StoreUpdate i d tree !! e) r
interpretStoreUpdateDb sem = do
  table <- queryTable
  interpretStoreUpdateDbWith table sem
{-# INLINE interpretStoreUpdateDb #-}
