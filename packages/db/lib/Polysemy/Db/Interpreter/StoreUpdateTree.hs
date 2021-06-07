module Polysemy.Db.Interpreter.StoreUpdateTree where

import qualified Polysemy.Db.Effect.StoreUpdateTree as StoreUpdateTree
import Polysemy.Db.Effect.StoreUpdateTree (StoreUpdateTree)
import Polysemy.Db.Data.Store (Store)
import qualified Polysemy.Db.Store as Store
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Tree.Data (GenDataTree, ReifyDataTree)
import Polysemy.Db.Tree.Partial (UpdatePartialTree, updatePartial)
import Polysemy.Db.Data.Uid (Uid)

type StrictStoreUpdateTree d tree dataTree =
  (
    GenDataTree d dataTree,
    ReifyDataTree dataTree d,
    UpdatePartialTree dataTree tree
  )

type UidStrictStoreUpdateTree i d tree dataTree =
  StrictStoreUpdateTree (Uid i d) tree dataTree

interpretStoreUpdateTreeStore ::
  ∀ i d e tree dataTree r .
  StrictStoreUpdateTree d tree dataTree =>
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
