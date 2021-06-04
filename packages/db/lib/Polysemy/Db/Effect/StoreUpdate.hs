module Polysemy.Db.Effect.StoreUpdate where

import Polysemy.Db.Data.Uid (Uid)
import Polysemy.Db.Tree.Partial (PartialTree)
import Polysemy.Db.Tree.Partial.Insert (FieldSpec, InsertPaths)

data StoreUpdate (i :: Type) (d :: Type) (fields :: [FieldSpec]) :: Effect where
  Partial :: i -> (∀ tree . InsertPaths d fields tree => PartialTree tree) -> StoreUpdate i d fields m ()

makeSem ''StoreUpdate

type UidStoreUpdate i d =
  StoreUpdate i (Uid i d)
