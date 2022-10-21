module Polysemy.Db.Tree.Data.TreeMeta where

import Fcf (Eval, Exp, type (@@))
import Fcf.Class.Functor (FMap)
import Generics.SOP (AllZip, AllZip2, HTrans (htrans), I (I), K (K), LiftedCoercible, NP, SOP, hcoerce)
import Prelude hiding (type (@@))

import Polysemy.Db.Data.FieldId (FieldId)
import Polysemy.Db.SOP.Constraint (Top2)

data TreeMeta =
  TreeMeta {
    name :: FieldId,
    rep :: Type,
    tpe :: Type
  }

type family TreeMetaType (meta :: TreeMeta) :: Type where
  TreeMetaType ('TreeMeta _ _ tpe) = tpe

data TreeMetaTypeF :: TreeMeta -> Exp Type
type instance Eval (TreeMetaTypeF meta) = TreeMetaType meta

type family TreeMetaTypes (metas :: [TreeMeta]) :: [Type] where
  TreeMetaTypes metas = FMap TreeMetaTypeF @@ metas

type family TreesMetaTypes (metas :: [[TreeMeta]]) :: [[Type]] where
  TreesMetaTypes '[] = '[]
  TreesMetaTypes (meta : metas) = TreeMetaTypes meta : TreesMetaTypes metas

data ConMeta =
  ConMeta {
    conNum :: Nat,
    conName :: FieldId,
    nodeMetas :: [TreeMeta]
  }

type family ConMetas (meta :: ConMeta) :: [TreeMeta] where
  ConMetas ('ConMeta _ _ metas) = metas

type family ConMetaTypes (meta :: ConMeta) :: [Type] where
  ConMetaTypes ('ConMeta _ _ nodes) = TreeMetaTypes nodes

type family ConsMetas (metas :: [ConMeta]) :: [[TreeMeta]] where
  ConsMetas '[] = '[]
  ConsMetas (meta : metas) = ConMetas meta : ConsMetas metas

type family ConsMetaTypes (metas :: [ConMeta]) :: [[Type]] where
  ConsMetaTypes '[] = '[]
  ConsMetaTypes (meta : metas) = ConMetaTypes meta : ConsMetaTypes metas

newtype TM (f :: Type -> Type) (meta :: TreeMeta) =
  TM { unTM :: f (TreeMetaType meta) }

deriving newtype instance Semigroup (f (TreeMetaType meta)) => Semigroup (TM f meta)

deriving newtype instance Monoid (f (TreeMetaType meta)) => Monoid (TM f meta)

-- TODO this can probably be simplified by moving the connection via TreeMetaTypes to callsites
class CoerceTM (f :: Type -> Type) (metas :: [TreeMeta]) where
  coerceTM :: NP f (TreeMetaTypes metas) -> NP (TM f) metas

instance {-# overlappable #-} (
    AllZip (LiftedCoercible f (TM f)) (TreeMetaTypes metas) metas
  ) => CoerceTM f metas where
    coerceTM =
      hcoerce

instance (
    AllZip Top2 (TreeMetaTypes metas) metas
  ) => CoerceTM (K a) metas where
    coerceTM =
      htrans (Proxy @Top2) \ (K x) -> TM (K x)

instance (
    AllZip TMTWitness (TreeMetaTypes metas) metas
  ) => CoerceTM I metas where
  coerceTM =
    htrans (Proxy @TMTWitness) \ (I x) -> TM (I x)

class TreeMetaType y ~ x => TMTWitness x y where
instance TreeMetaType y ~ x => TMTWitness x y where

class CoerceTM2 (f :: Type -> Type) (metass :: [[TreeMeta]]) where
  coerceTM2 :: SOP f (TreesMetaTypes metass) -> SOP (TM f) metass

instance (
    AllZip2 Top2 (TreesMetaTypes metass) metass
  ) => CoerceTM2 (K a) metass where
    coerceTM2 =
      htrans (Proxy @Top2) \ (K x) -> TM (K x)

instance (
    AllZip2 TMTWitness (TreesMetaTypes metass) metass
  ) => CoerceTM2 I metass where
    coerceTM2 =
      htrans (Proxy @TMTWitness) \ (I x) -> TM (I x)
