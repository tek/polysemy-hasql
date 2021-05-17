module Polysemy.Db.Tree.Fold where

import Generics.SOP (All, hcfoldMap)

import Polysemy.Db.Data.FieldId (FieldId)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import qualified Polysemy.Db.Type.Data.Tree as Type

class FoldTreePrim (t :: Type) (n :: Type -> Type) (m :: Type) (name :: FieldId) (effs :: [*]) (d :: Type) where
  foldTreePrim :: n d -> m

class FoldTree (t :: Type) (n :: Type -> Type) (m :: Type) (tree :: Kind.Tree) where
  foldTree :: Type.Tree t n tree -> m

instance (
    FoldTreePrim t n m name effs d
  ) => FoldTree t n m ('Kind.Tree name effs ('Kind.Prim d)) where
  foldTree (Type.Tree _ (Type.Prim nd)) =
    foldTreePrim @t @n @m @name @effs @d nd

-- TODO also collect result for this
instance (
    Monoid m,
    All (FoldTree t n m) trees
  ) => FoldTree t n m ('Kind.Tree name effs ('Kind.Prod d trees)) where
  foldTree (Type.Tree _ (Type.Prod _ trees)) =
    hcfoldMap (Proxy @(FoldTree t n m)) foldTree trees
