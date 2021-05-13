module Polysemy.Db.Tree.Api where

import Generics.SOP (HPure, I, NP, SOP, hpure, unI, unSOP, unZ)
import Generics.SOP.Constraint (SListIN)
import Generics.SOP.GGP (gfrom)

import Polysemy.Db.Data.FieldId (FieldId)
import Polysemy.Db.SOP.Constraint (ConstructSOP)
import Polysemy.Db.SOP.List (NPAsUnit (npAsUnit))
import Polysemy.Db.Tree.Data.TreeMeta (
  CoerceTM (coerceTM),
  CoerceTM2 (coerceTM2),
  TM(TM),
  TreeMeta,
  TreeMetaTypes,
  TreesMetaTypes,
  )

class TreePrim (tag :: Type) (n :: Type -> Type) (name :: FieldId) (d :: Type) where
  treePrim :: n d -> n d

instance {-# overlappable #-} TreePrim tag n name d where
  treePrim =
    id

class TreePayload (tag :: Type) (meta :: TreeMeta) (effs :: [Type]) (t :: Type) where
  treePayload :: t

instance {-# overlappable #-} TreePayload tag meta effs () where
  treePayload =
    ()

type TreeSOPT k = (TreeMeta -> Type) -> [k] -> Type

class TreeSOP (tag :: Type) (metas :: [k]) (h :: TreeSOPT k) (f :: Type -> Type) (d :: Type) where
  treeSOP :: f d -> h (TM f) metas

instance {-# overlappable #-} (
    SListIN h metas,
    HPure h,
    Alternative f
  ) => TreeSOP tag metas h f d where
    treeSOP _ =
      hpure (TM empty)

instance {-# overlappable #-} (
    CoerceTM I metas,
    ConstructSOP d '[TreeMetaTypes metas]
  ) => TreeSOP tag metas NP I d where
    treeSOP =
      coerceTM . unZ . unSOP . gfrom . unI

instance {-# overlappable #-} (
    ConstructSOP d (TreesMetaTypes metas),
    CoerceTM2 I metas
  ) => TreeSOP tag metas SOP I d where
    treeSOP =
      coerceTM2 @I @metas . gfrom . unI

class TreeConProduct (tag :: Type) (f :: Type -> Type) (metas :: [TreeMeta]) (d :: Type) where
  treeConProduct :: d -> NP f (TreeMetaTypes metas)

instance {-# overlappable #-} (
    NPAsUnit f metas (TreeMetaTypes metas)
  ) => TreeConProduct tag f metas () where
    treeConProduct _ =
      npAsUnit @_ @f @metas

class TreeConPayload (tag :: Type) (field :: FieldId) (t :: Type) where
  treeConPayload :: t

instance TreeConPayload tag name () where
  treeConPayload =
    ()
