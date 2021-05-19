module Polysemy.Db.Tree.Unfold where

import Generics.SOP (All, HSequence (hctraverse'), NP)

import qualified Polysemy.Db.Kind.Data.Tree as Kind
import qualified Polysemy.Db.Type.Data.Tree as Type

class UnfoldTreePrim (t :: Type) (n :: Type -> Type) (f :: Type -> Type) (env :: Type) (tree :: Kind.Tree) (d :: Type) where
  unfoldTreePrim :: env -> f (n d)

class UnfoldTreeExtract (t :: Type) (n :: Type -> Type) (env :: Type) (tree :: Kind.Tree) where
  unfoldTreeExtract :: env -> env

unfoldTrees ::
  ∀ t n f env trees .
  Applicative f =>
  All (UnfoldTree t n f env) trees =>
  env ->
  NP (Type.Tree t n) trees ->
  f (NP (Type.Tree t n) trees)
unfoldTrees env trees =
  hctraverse' (Proxy @(UnfoldTree t n f env)) (unfoldTree env) trees

class UnfoldCon (t :: Type) (n :: Type -> Type) (f :: Type -> Type) (env :: Type) (con :: Kind.Con) where
  unfoldCon :: env -> Type.Con t n con -> f (Type.Con t n con)

instance (
    Applicative f,
    All (UnfoldTree t n f env) trees
  ) => UnfoldCon t n f env ('Kind.Con name trees) where
  unfoldCon env (Type.Con trees) =
    Type.Con <$> unfoldTrees @t @n @f @env @trees env trees

instance (
    Functor f,
    UnfoldTree t n f env tree
  ) => UnfoldCon t n f env ('Kind.ConUna name tree) where
  unfoldCon env (Type.ConUna tree) =
    Type.ConUna <$> unfoldTree @t @n @f @env @tree env tree

class UnfoldTree (t :: Type) (n :: Type -> Type) (f :: Type -> Type) (env :: Type) (tree :: Kind.Tree) where
  unfoldTree :: env -> Type.Tree t n tree -> f (Type.Tree t n tree)

instance (
    Functor f,
    tree ~ 'Kind.Tree name effs ('Kind.Prim d),
    UnfoldTreePrim t n f env tree d
  ) => UnfoldTree t n f env ('Kind.Tree name effs ('Kind.Prim d)) where
  unfoldTree env (Type.Tree t (Type.Prim _)) =
    Type.Tree t . Type.Prim <$> payload
    where
      payload =
        unfoldTreePrim @t @n @f @env @tree @d env

instance (
    Applicative f,
    tree ~ 'Kind.Tree name effs ('Kind.Prod d trees),
    UnfoldTreeExtract t n env tree,
    All (UnfoldTree t n f env) trees
  ) => UnfoldTree t n f env ('Kind.Tree name effs ('Kind.Prod d trees)) where
  unfoldTree env (Type.Tree t (Type.Prod n trees)) =
    Type.Tree t . Type.Prod n <$> unfoldTrees @t @n @f subEnv trees
    where
      subEnv =
        unfoldTreeExtract @t @n @env @tree env

-- instance (
--     UnfoldTreeExtract t n m name effs,
--     All (UnfoldCon t n m) cons
--   ) => UnfoldTree t n m ('Kind.Tree name effs ('Kind.Sum d cons)) where
--   unfoldTree (Type.Tree _ (Type.Sum _ cons)) =
--     unfoldTreeExtract @t @n @m @name @effs (pure (hcollapse (hcmap (Proxy @(UnfoldCon t n m)) (K . unfoldCon) cons)))

class UnfoldRoot (t :: Type) (n :: Type -> Type) (f :: Type -> Type) (env :: Type) (tree :: Kind.Tree) where
  unfoldRoot :: env -> Type.Tree t n tree -> f (Type.Tree t n tree)

instance (
    Applicative f,
    tree ~ 'Kind.Tree name effs ('Kind.Prod d trees),
    All (UnfoldTree t n f env) trees
  ) => UnfoldRoot t n f env ('Kind.Tree name effs ('Kind.Prod d trees)) where
  unfoldRoot env (Type.Tree t (Type.Prod n trees)) =
    Type.Tree t . Type.Prod n <$> unfoldTrees @t @n @f env trees

instance (
    Applicative f,
    tree ~ 'Kind.Tree name effs ('Kind.SumProd d cons),
    All (UnfoldCon t n f env) cons
  ) => UnfoldRoot t n f env ('Kind.Tree name effs ('Kind.SumProd d cons)) where
  unfoldRoot env (Type.Tree t (Type.SumProd n cons)) =
    Type.Tree t . Type.SumProd n <$> hctraverse' (Proxy @(UnfoldCon t n f env)) (unfoldCon env) cons
