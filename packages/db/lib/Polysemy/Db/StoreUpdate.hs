module Polysemy.Db.StoreUpdate where

import Polysemy.Db.Data.Store (Store)
import qualified Polysemy.Db.Data.StoreUpdate as StoreUpdate
import Polysemy.Db.Data.StoreUpdate (StoreUpdate)
import qualified Polysemy.Db.Store as Store
import Polysemy.Db.Tree.Data (GenDataTree, ReifyDataTree)
import Polysemy.Db.Tree.Partial (Partial, UpdatePartialTree, partial, updatePartial)

interpretStoreUpdateStore ::
  ∀ i d e tree dataTree r .
  GenDataTree d dataTree =>
  ReifyDataTree dataTree d =>
  UpdatePartialTree dataTree tree =>
  Partial d tree =>
  Member (Store i d !! e) r =>
  InterpreterFor (StoreUpdate i d tree !! e) r
interpretStoreUpdateStore =
  interpretResumable \case
    StoreUpdate.Update i f ->
      restop @e @(Store i d) (Store.alter i (updatePartial @d (f (partial @d))))
