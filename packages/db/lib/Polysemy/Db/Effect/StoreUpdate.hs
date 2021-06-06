module Polysemy.Db.Effect.StoreUpdate where

import Polysemy.Db.Data.Uid (Uid)
import Polysemy.Db.Tree.Partial.Insert (FieldSpec, PartialUpdate)

data StoreUpdate (i :: Type) (d :: Type) (fields :: [FieldSpec]) :: Effect where
  Partial :: i -> PartialUpdate d fields -> StoreUpdate i d fields m ()

makeSem ''StoreUpdate

type UidStoreUpdate i d =
  StoreUpdate i (Uid i d)