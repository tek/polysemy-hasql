module Polysemy.Db.Tree.FoldMap where

import Generics.SOP (All, HCollapse (hcollapse), K (K), hcmap)

import Polysemy.Db.Data.FieldId (FieldId)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import qualified Polysemy.Db.Type.Data.Tree as Type

class FoldMapTreePrim (root :: Bool) (t :: Type) (n :: Type -> Type) (m :: Type) (name :: FieldId) (effs :: [Type]) (d :: Type) where
  foldMapTreePrim :: n d -> m

class FoldMapTreeConcat (root :: Bool) (t :: Type) (n :: Type -> Type) (m :: Type) (name :: FieldId) (effs :: [Type]) where
  foldMapTreeConcat :: [m] -> m

instance {-# overlappable #-} (
    Monoid m
  ) => FoldMapTreeConcat root t n m name effs where
  foldMapTreeConcat =
    mconcat

class FoldMapCon (t :: Type) (n :: Type -> Type) (m :: Type) (con :: Kind.Con) where
  foldCon :: Type.Con t n con -> m

instance (
    FoldMapTreeConcat 'False t n m name '[],
    All (FoldMapTree 'False t n m) trees
  ) => FoldMapCon t n m ('Kind.Con num name trees) where
  foldCon (Type.Con trees) =
    foldMapTreeConcat @'False @t @n @m @name @'[] (hcollapse (hcmap (Proxy @(FoldMapTree 'False t n m)) (K . foldMapTree @'False) trees))

instance (
    FoldMapTree 'False t n m tree
  ) => FoldMapCon t n m ('Kind.ConUna num name tree) where
  foldCon (Type.ConUna tree) =
    foldMapTree @'False tree

class FoldMapTree (root :: Bool) (t :: Type) (n :: Type -> Type) (m :: Type) (tree :: Kind.Tree) where
  foldMapTree :: Type.Tree t n tree -> m

instance (
    FoldMapTreePrim root t n m name effs d
  ) => FoldMapTree root t n m ('Kind.Tree name effs ('Kind.Prim d)) where
  foldMapTree (Type.Tree _ (Type.Prim nd)) =
    foldMapTreePrim @root @t @n @m @name @effs @d nd

-- TODO also collect result for this
instance (
    FoldMapTreeConcat root t n m name effs,
    All (FoldMapTree 'False t n m) trees
  ) => FoldMapTree root t n m ('Kind.Tree name effs ('Kind.Prod d trees)) where
  foldMapTree (Type.Tree _ (Type.Prod _ trees)) =
    foldMapTreeConcat @root @t @n @m @name @effs (hcollapse (hcmap (Proxy @(FoldMapTree 'False t n m)) (K . foldMapTree @'False) trees))

instance (
    FoldMapTreeConcat root t n m name effs,
    All (FoldMapCon t n m) cons
  ) => FoldMapTree root t n m ('Kind.Tree name effs ('Kind.SumProd d cons)) where
  foldMapTree (Type.Tree _ (Type.SumProd _ trees)) =
    foldMapTreeConcat @root @t @n @m @name @effs (hcollapse (hcmap (Proxy @(FoldMapCon t n m)) (K . foldCon) trees))

instance (
    FoldMapTreeConcat root t n m name effs,
    All (FoldMapCon t n m) cons
  ) => FoldMapTree root t n m ('Kind.Tree name effs ('Kind.Sum d cons)) where
  foldMapTree (Type.Tree _ (Type.Sum _ cons)) =
    foldMapTreeConcat @root @t @n @m @name @effs (pure (hcollapse (hcmap (Proxy @(FoldMapCon t n m)) (K . foldCon) cons)))
