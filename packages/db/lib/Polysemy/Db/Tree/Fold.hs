module Polysemy.Db.Tree.Fold where

import Generics.SOP (All, hcfoldMap)

import Polysemy.Db.Data.FieldId (FieldId)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import qualified Polysemy.Db.Type.Data.Tree as Type

class FoldTreePrim (t :: Type) (n :: Type -> Type) (m :: Type) (name :: FieldId) (effs :: [*]) (d :: Type) where
  foldTreePrim :: n d -> m

class FoldCon (t :: Type) (n :: Type -> Type) (m :: Type) (con :: Kind.Con) where
  foldCon :: Type.Con t n con -> m

instance (
    Monoid m,
    All (FoldTree t n m) trees
  ) => FoldCon t n m ('Kind.Con name trees) where
  foldCon (Type.Con trees) =
    hcfoldMap (Proxy @(FoldTree t n m)) foldTree trees

instance (
    FoldTree t n m tree
  ) => FoldCon t n m ('Kind.ConUna name tree) where
  foldCon (Type.ConUna tree) =
    foldTree tree

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

instance (
    Monoid m,
    All (FoldCon t n m) cons
  ) => FoldTree t n m ('Kind.Tree name effs ('Kind.Sum d cons)) where
  foldTree (Type.Tree _ (Type.Sum _ cons)) =
    hcfoldMap (Proxy @(FoldCon t n m)) foldCon cons
