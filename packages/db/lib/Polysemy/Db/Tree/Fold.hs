module Polysemy.Db.Tree.Fold where

import Generics.SOP (All, HCollapse (hcollapse), K(K), hcmap)

import Polysemy.Db.Data.FieldId (FieldId)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import qualified Polysemy.Db.Type.Data.Tree as Type

class FoldTreePrim (root :: Bool) (t :: Type) (n :: Type -> Type) (m :: Type) (name :: FieldId) (effs :: [Type]) (d :: Type) where
  foldTreePrim :: n d -> m

class FoldTreeConcat (root :: Bool) (t :: Type) (n :: Type -> Type) (m :: Type) (name :: FieldId) (effs :: [Type]) where
  foldTreeConcat :: [m] -> m

instance {-# overlappable #-} (
    Monoid m
  ) => FoldTreeConcat root t n m name effs where
  foldTreeConcat =
    mconcat

class FoldCon (t :: Type) (n :: Type -> Type) (m :: Type) (con :: Kind.Con) where
  foldCon :: Type.Con t n con -> m

instance (
    FoldTreeConcat 'False t n m name '[],
    All (FoldTree 'False t n m) trees
  ) => FoldCon t n m ('Kind.Con num name trees) where
  foldCon (Type.Con trees) =
    foldTreeConcat @'False @t @n @m @name @'[] (hcollapse (hcmap (Proxy @(FoldTree 'False t n m)) (K . foldTree @'False) trees))

instance (
    FoldTree 'False t n m tree
  ) => FoldCon t n m ('Kind.ConUna num name tree) where
  foldCon (Type.ConUna tree) =
    foldTree @'False tree

class FoldTree (root :: Bool) (t :: Type) (n :: Type -> Type) (m :: Type) (tree :: Kind.Tree) where
  foldTree :: Type.Tree t n tree -> m

instance (
    FoldTreePrim root t n m name effs d
  ) => FoldTree root t n m ('Kind.Tree name effs ('Kind.Prim d)) where
  foldTree (Type.Tree _ (Type.Prim nd)) =
    foldTreePrim @root @t @n @m @name @effs @d nd

-- TODO also collect result for this
instance (
    FoldTreeConcat root t n m name effs,
    All (FoldTree 'False t n m) trees
  ) => FoldTree root t n m ('Kind.Tree name effs ('Kind.Prod d trees)) where
  foldTree (Type.Tree _ (Type.Prod _ trees)) =
    foldTreeConcat @root @t @n @m @name @effs (hcollapse (hcmap (Proxy @(FoldTree 'False t n m)) (K . foldTree @'False) trees))

instance (
    FoldTreeConcat root t n m name effs,
    All (FoldCon t n m) cons
  ) => FoldTree root t n m ('Kind.Tree name effs ('Kind.SumProd d cons)) where
  foldTree (Type.Tree _ (Type.SumProd _ trees)) =
    foldTreeConcat @root @t @n @m @name @effs (hcollapse (hcmap (Proxy @(FoldCon t n m)) (K . foldCon) trees))

instance (
    FoldTreeConcat root t n m name effs,
    All (FoldCon t n m) cons
  ) => FoldTree root t n m ('Kind.Tree name effs ('Kind.Sum d cons)) where
  foldTree (Type.Tree _ (Type.Sum _ cons)) =
    foldTreeConcat @root @t @n @m @name @effs (pure (hcollapse (hcmap (Proxy @(FoldCon t n m)) (K . foldCon) cons)))
