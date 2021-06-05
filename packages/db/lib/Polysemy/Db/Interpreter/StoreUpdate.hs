module Polysemy.Db.Interpreter.StoreUpdate where

import Polysemy.Db.Data.Store (Store)
import qualified Polysemy.Db.Effect.StoreUpdate as StoreUpdate
import Polysemy.Db.Effect.StoreUpdate (StoreUpdate)
import qualified Polysemy.Db.Store as Store
import Polysemy.Db.Tree.Data (GenDataTree, ReifyDataTree)
import Polysemy.Db.Tree.Partial (UpdatePartialTree, updatePartial)
import Polysemy.Db.Tree.Partial.Insert (InsertPaths, PartialUpdate (PartialUpdate))

interpretStoreUpdateStore ::
  ∀ i d e fields tree dataTree r .
  GenDataTree d dataTree =>
  ReifyDataTree dataTree d =>
  UpdatePartialTree dataTree tree =>
  InsertPaths d fields tree =>
  Member (Store i d !! e) r =>
  InterpreterFor (StoreUpdate i d fields !! e) r
interpretStoreUpdateStore =
  interpretResumable \case
    StoreUpdate.Partial i (PartialUpdate t) ->
      restop @e @(Store i d) (Store.alter i (updatePartial @d t))
