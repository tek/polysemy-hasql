module Polysemy.Db.Data.StoreUpdate where

import Polysemy.Db.Data.Uid (Uid)
import Polysemy.Db.Tree.Partial (PartialTree)

data StoreUpdate i d tree :: Effect where
  Update :: i -> (PartialTree tree -> PartialTree tree) -> StoreUpdate i d tree m ()

makeSem ''StoreUpdate

type UidStoreUpdate i d =
  StoreUpdate i (Uid i d)
