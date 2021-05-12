module Polysemy.Db.Tree.Api where

import Generics.SOP (I, K(K), NP, POP, SListI, SListI2, SOP, hindex, hpure, unI, unSOP, unZ)
import Generics.SOP.GGP (gfrom)

import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import Polysemy.Db.SOP.Constraint (ConstructSOP)
import Polysemy.Db.SOP.List (NPAsUnit (npAsUnit))
import Polysemy.Db.Tree.Data.TreeMeta (
  CoerceTM (coerceTM),
  CoerceTM2 (coerceTM2),
  ConMeta,
  ConsMetas,
  TM(TM),
  TreeMeta,
  TreeMetaTypes,
  TreesMetaTypes,
  )

class TreePrim (tag :: Type) (n :: Type -> Type) (a :: Type) (name :: FieldId) (d :: Type) where
  treePrim :: a -> n d

instance {-# overlappable #-} Applicative f => TreePrim tag f d name d where
  treePrim =
    pure

instance {-# overlappable #-} TreePrim tag f (f d) name d where
  treePrim =
    id

instance {-# incoherent #-} (
    Applicative f,
    ConstructSOP a ass
  ) => TreePrim tag f a ('NamedField "sum_index") Int where
  treePrim =
    pure . hindex . gfrom

class TreePayload (tag :: Type) (t :: Type) (a :: Type) (meta :: TreeMeta) (effs :: [Type]) where
  treePayload :: a -> t

instance {-# overlappable #-} TreePayload tag () a meta effs where
  treePayload _ =
    ()

class TreeProduct (tag :: Type) (f :: Type -> Type) (metas :: [TreeMeta]) (d :: Type) where
  treeProduct :: d -> NP (TM f) metas

instance {-# overlappable #-} SListI metas => TreeProduct tag (K ()) metas (K () d) where
  treeProduct _ =
    hpure (TM (K ()))

instance (
    CoerceTM I metas,
    ds ~ TreeMetaTypes metas,
    ConstructSOP d '[ds]
  ) => TreeProduct tag I metas (I d) where
    treeProduct =
      coerceTM . unZ . unSOP . gfrom . unI

class TreeSumProd (tag :: Type) (f :: Type -> Type) (metas :: [ConMeta]) (d :: Type) where
  treeSumProd :: f d -> POP (TM f) (ConsMetas metas)

instance (
    SListI2 (ConsMetas metas)
  ) => TreeSumProd tag (K ()) metas d where
  treeSumProd _ =
    hpure (TM (K ()))

class TreeSum (tag :: Type) (f :: Type -> Type) (metas :: [ConMeta]) (d :: Type) where
  treeSum :: f d -> SOP (TM f) (ConsMetas metas)

instance {-# overlappable #-} (
    ConstructSOP d (TreesMetaTypes (ConsMetas metas)),
    CoerceTM2 I (ConsMetas metas)
  ) => TreeSum tag I metas d where
  treeSum =
    coerceTM2 @I @(ConsMetas metas) . gfrom . unI

class TreeConProduct (tag :: Type) (f :: Type -> Type) (metas :: [TreeMeta]) (d :: Type) where
  treeConProduct :: d -> NP f (TreeMetaTypes metas)

instance {-# overlappable #-} (
    NPAsUnit f metas (TreeMetaTypes metas)
  ) => TreeConProduct tag f metas () where
  treeConProduct _ =
    npAsUnit @_ @f @metas

class TreeConPayload (tag :: Type) (field :: FieldId) (t :: Type) where
  treeConPayload :: t
