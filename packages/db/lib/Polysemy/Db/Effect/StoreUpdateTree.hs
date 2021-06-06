module Polysemy.Db.Effect.StoreUpdateTree where

import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree.Partial (PartialTree)

data StoreUpdateTree (i :: Type) (d :: Type) (tree :: Kind.Tree) :: Effect where
  Partial :: i -> PartialTree tree -> StoreUpdateTree i d tree m (Maybe d)

makeSem ''StoreUpdateTree
