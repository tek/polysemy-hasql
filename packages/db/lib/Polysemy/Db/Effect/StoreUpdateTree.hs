module Polysemy.Db.Effect.StoreUpdateTree where

import Polysemy.Db.Data.Uid (Uid)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree.Partial (PartialTree)

data StoreUpdateTree (i :: Type) (d :: Type) (tree :: Kind.Tree) :: Effect where
  Partial :: i -> PartialTree tree -> StoreUpdateTree i d tree m (Maybe (Uid i d))

makeSem ''StoreUpdateTree
