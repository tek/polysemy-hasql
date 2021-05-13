module Polysemy.Db.Tree.Api where

import Generics.SOP (I, K(K), NP, POP, SListI, SListI2, SOP, hpure, unI, unSOP, unZ)
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

class TreePrim (tag :: Type) (n :: Type -> Type) (f :: Type -> Type) (name :: FieldId) (d :: Type) where
  treePrim :: f d -> n d

-- instance {-# overlappable #-} Applicative f => TreePrim tag n f name d where
--   treePrim =
--     pure

instance {-# overlappable #-} TreePrim tag n n name d where
  treePrim =
    id

-- instance {-# incoherent #-} (
--     Applicative f,
--     ConstructSOP a ass
--   ) => TreePrim tag n f ('NamedField "sum_index") Int where
--     treePrim =
--       pure . hindex . gfrom

class TreePayload (tag :: Type) (t :: Type) (a :: Type) (meta :: TreeMeta) (effs :: [Type]) where
  treePayload :: a -> t

instance {-# overlappable #-} TreePayload tag () a meta effs where
  treePayload _ =
    ()

type TreeSOPT k = (TreeMeta -> Type) -> [k] -> Type

class TreeSOP (tag :: Type) (metas :: [k]) (h :: TreeSOPT k) (f :: Type -> Type) (d :: Type) where
  treeSOP :: f d -> h (TM f) metas

instance {-# overlappable #-} (
    SListI metas
  ) => TreeSOP tag metas NP (K ()) d where
    treeSOP _ =
      hpure (TM (K ()))

instance {-# overlappable #-} (
    CoerceTM I metas,
    ConstructSOP d '[TreeMetaTypes metas]
  ) => TreeSOP tag metas NP I d where
    treeSOP =
      coerceTM . unZ . unSOP . gfrom . unI

instance {-# overlappable #-} (
    SListI2 metas
  ) => TreeSOP tag metas POP (K ()) d where
    treeSOP _ =
      hpure (TM (K ()))

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
