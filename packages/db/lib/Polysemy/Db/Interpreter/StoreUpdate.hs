module Polysemy.Db.Interpreter.StoreUpdate where

import Polysemy.Db.Data.PartialField (Partial (Partial))
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)
import qualified Polysemy.Db.Effect.StoreUpdate as StoreUpdate
import Polysemy.Db.Effect.StoreUpdate (StoreUpdate)
import qualified Polysemy.Db.Store as Store
import Polysemy.Db.Tree.Data (GenDataTree, ReifyDataTree)
import Polysemy.Db.Tree.Partial (UpdatePartialTree, updatePartial)
import Polysemy.Db.Tree.Partial.Insert (InsertPaths, PartialUpdate (PartialUpdate))
import Polysemy.Db.Data.Uid (Uid)

type StrictStoreUpdate d fields tree dataTree =
  (
    GenDataTree d dataTree,
    ReifyDataTree dataTree d,
    UpdatePartialTree dataTree tree,
    InsertPaths d fields tree
  )

type UidStrictStoreUpdate i d fields tree dataTree =
  StrictStoreUpdate (Uid i d) fields tree dataTree

interpretStoreUpdateStore ::
  ∀ i d e fields tree dataTree r .
  StrictStoreUpdate d fields tree dataTree =>
  Member (Store i d !! e) r =>
  InterpreterFor (StoreUpdate i d fields !! e) r
interpretStoreUpdateStore =
  interpretResumable \case
    StoreUpdate.Create i (PartialUpdate t) -> do
      restop @e @(Store i d) do
        (Store.alter i (updatePartial @d t))
        Store.fetch i
    StoreUpdate.Use i (Partial t) -> do
      restop @e @(Store i d) do
        (Store.alter i (updatePartial @d t))
        Store.fetch i

interpretStoreUpdateNull ::
  InterpreterFor (StoreUpdate i d paths !! e) r
interpretStoreUpdateNull =
  interpretResumable \case
    StoreUpdate.Create _ _ ->
      pure Nothing
    StoreUpdate.Use _ _ ->
      pure Nothing
{-# inline interpretStoreUpdateNull #-}
