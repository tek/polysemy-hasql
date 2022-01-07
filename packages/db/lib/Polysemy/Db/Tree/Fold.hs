module Polysemy.Db.Tree.Fold where

import Generics.SOP (All, HCollapse (hcollapse), K (K), hcmap)

import Polysemy.Db.Data.FieldId (FieldId)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import qualified Polysemy.Db.Type.Data.Tree as Type

class FoldTreePrim (root :: Bool) (t :: Type) (n :: Type -> Type) (env :: Type) (m :: Type) (name :: FieldId) (effs :: [Type]) (d :: Type) where
  foldTreePrim :: env -> n d -> m

class FoldTreeConcat (root :: Bool) (t :: Type) (n :: Type -> Type) (m :: Type) (name :: FieldId) (effs :: [Type]) where
  foldTreeConcat :: [m] -> m

instance {-# overlappable #-} (
    Monoid m
  ) => FoldTreeConcat root t n m name effs where
  foldTreeConcat =
    mconcat

class FoldTreeLocal (t :: Type) (n :: Type -> Type) (env :: Type) (m :: Type) (name :: FieldId) (effs :: [Type]) (node :: Kind.Node) where
  foldTreeLocal :: env -> env

class FoldTreeLocalCon (t :: Type) (n :: Type -> Type) (env :: Type) (m :: Type) (name :: FieldId) (effs :: [Type]) (node :: Kind.Con) where
  foldTreeLocalCon :: env -> env

class FoldCon (t :: Type) (n :: Type -> Type) (env :: Type) (m :: Type) (con :: Kind.Con) where
  foldCon :: env -> Type.Con t n con -> m

instance (
    FoldTreeLocalCon t n env m name '[] ('Kind.Con num name trees),
    FoldTreeConcat 'False t n m name '[],
    All (FoldTree 'False t n env m) trees
  ) => FoldCon t n env m ('Kind.Con num name trees) where
  foldCon env (Type.Con trees) =
    foldTreeConcat @'False @t @n @m @name @'[] (hcollapse (hcmap (Proxy @(FoldTree 'False t n env m)) (K . foldTree @'False subEnv) trees))
    where
      subEnv =
        foldTreeLocalCon @t @n @env @m @name @'[] @('Kind.Con num name trees) env

instance (
    FoldTreeLocalCon t n env m name '[] ('Kind.ConUna num name tree),
    FoldTree 'False t n env m tree
  ) => FoldCon t n env m ('Kind.ConUna num name tree) where
  foldCon env (Type.ConUna tree) =
    foldTree @'False subEnv tree
    where
      subEnv =
        foldTreeLocalCon @t @n @env @m @name @'[] @('Kind.ConUna num name tree) env

class FoldTree (root :: Bool) (t :: Type) (n :: Type -> Type) (env :: Type) (m :: Type) (tree :: Kind.Tree) where
  foldTree :: env -> Type.Tree t n tree -> m

instance (
    FoldTreePrim root t n env m name effs d
  ) => FoldTree root t n env m ('Kind.Tree name effs ('Kind.Prim d)) where
  foldTree env (Type.Tree _ (Type.Prim nd)) =
    foldTreePrim @root @t @n @env @m @name @effs @d env nd

-- TODO also collect result for this
instance (
    FoldTreeLocal t n env m name effs ('Kind.Prod d trees),
    FoldTreeConcat root t n m name effs,
    All (FoldTree 'False t n env m) trees
  ) => FoldTree root t n env m ('Kind.Tree name effs ('Kind.Prod d trees)) where
  foldTree env (Type.Tree _ (Type.Prod _ trees)) =
    foldTreeConcat @root @t @n @m @name @effs (hcollapse (hcmap (Proxy @(FoldTree 'False t n env m)) (K . foldTree @'False subEnv) trees))
    where
      subEnv =
        foldTreeLocal @t @n @env @m @name @effs @('Kind.Prod d trees) env

instance (
    FoldTreeLocal t n env m name effs ('Kind.SumProd d cons),
    FoldTreeConcat root t n m name effs,
    All (FoldCon t n env m) cons
  ) => FoldTree root t n env m ('Kind.Tree name effs ('Kind.SumProd d cons)) where
  foldTree env (Type.Tree _ (Type.SumProd _ trees)) =
    foldTreeConcat @root @t @n @m @name @effs (hcollapse (hcmap (Proxy @(FoldCon t n env m)) (K . foldCon subEnv) trees))
    where
      subEnv =
        foldTreeLocal @t @n @env @m @name @effs @('Kind.SumProd d cons) env

instance (
    FoldTreeLocal t n env m name effs ('Kind.Sum d cons),
    FoldTreeConcat root t n m name effs,
    All (FoldCon t n env m) cons
  ) => FoldTree root t n env m ('Kind.Tree name effs ('Kind.Sum d cons)) where
  foldTree env (Type.Tree _ (Type.Sum _ cons)) =
    foldTreeConcat @root @t @n @m @name @effs (pure (hcollapse (hcmap (Proxy @(FoldCon t n env m)) (K . foldCon subEnv) cons)))
    where
      subEnv =
        foldTreeLocal @t @n @env @m @name @effs @('Kind.Sum d cons) env
