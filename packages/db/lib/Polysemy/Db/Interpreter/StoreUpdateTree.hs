module Polysemy.Db.Interpreter.StoreUpdateTree where

import qualified Polysemy.Db.Effect.StoreUpdateTree as StoreUpdateTree
import Polysemy.Db.Effect.StoreUpdateTree (StoreUpdateTree)
import Polysemy.Db.Data.Store (Store)
import qualified Polysemy.Db.Store as Store
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Tree.Data (GenDataTree, ReifyDataTree)
import Polysemy.Db.Tree.Partial (UpdatePartialTree, updatePartial)

interpretStoreUpdateTreeStore ::
  ∀ i d e tree dataTree r .
  GenDataTree d dataTree =>
  ReifyDataTree dataTree d =>
  UpdatePartialTree dataTree tree =>
  Member (Store i d !! e) r =>
  InterpreterFor (StoreUpdateTree i d tree !! e) r
interpretStoreUpdateTreeStore =
  interpretResumable \case
    StoreUpdateTree.Partial i t -> do
      restop @e @(Store i d) do
        (Store.alter i (updatePartial @d t))
        Store.fetch i
{-# inline interpretStoreUpdateTreeStore #-}

interpretStoreUpdateTreeNull ::
  InterpreterFor (StoreUpdateTree i d paths !! e) r
interpretStoreUpdateTreeNull =
  interpretResumable \case
    StoreUpdateTree.Partial _ _ ->
      pure Nothing
{-# inline interpretStoreUpdateTreeNull #-}
