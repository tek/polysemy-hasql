module Polysemy.Db.Effect.StoreUpdate where

import Polysemy.Db.Data.Partial (Partial)
import Polysemy.Db.Data.PartialUpdate (PartialUpdate)
import Polysemy.Db.Data.Uid (Uid)
import Polysemy.Db.Tree.Partial.Insert (FieldSpec)

data StoreUpdate (i :: Type) (d :: Type) (fields :: [FieldSpec]) :: Effect where
  Create :: i -> PartialUpdate d fields -> StoreUpdate i d fields m (Maybe (Uid i d))
  Use :: i -> Partial d -> StoreUpdate i d fields m (Maybe (Uid i d))

makeSem ''StoreUpdate
