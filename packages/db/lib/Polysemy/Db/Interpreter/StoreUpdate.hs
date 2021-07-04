module Polysemy.Db.Interpreter.StoreUpdate where

import Polysemy.Db.Data.Partial (Partial (Partial))
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)
import qualified Polysemy.Db.Effect.StoreUpdate as StoreUpdate
import Polysemy.Db.Effect.StoreUpdate (StoreUpdate)
import qualified Polysemy.Db.Store as Store
import Polysemy.Db.Store (StrictStoreUpdate)
import Polysemy.Db.Tree.Partial (updatePartial)
import Polysemy.Db.Tree.Partial.Insert (PartialUpdate (PartialUpdate))

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
{-# inline interpretStoreUpdateStore #-}

interpretStoreUpdateNull ::
  InterpreterFor (StoreUpdate i d paths !! e) r
interpretStoreUpdateNull =
  interpretResumable \case
    StoreUpdate.Create _ _ ->
      pure Nothing
    StoreUpdate.Use _ _ ->
      pure Nothing
{-# inline interpretStoreUpdateNull #-}
