module Polysemy.Db.Tree.Data where

import Generics.SOP (I(I), NP, NS (Z), SOP(SOP), hindex, htrans, unSOP, unZ, All, Top, AllZip)
import Generics.SOP.GGP (GCode, gfrom, gto)

import Polysemy.Db.Data.Column (Auto)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Constraint (ConstructSOP, ReifySOP)
import Polysemy.Db.Tree (
  Params(Params),
  RootName,
  SumOrProd,
  Tree(..),
  TreeConPayload(..),
  TreePrim(..),
  TreeProduct(..),
  )
import Polysemy.Db.Tree.Effect (DefaultEffects, TreeEffects)
import Polysemy.Db.Tree.Meta (TreeMeta(TreeMeta), TreeMetaType)
import qualified Polysemy.Db.Type.Data.Tree as Type

data DataTag =
  DataTag
  deriving (Eq, Show)

type DataTree = Type.Tree () Identity
type DataNode = Type.Node () Identity
type DataParams = 'Params DataTag () Identity

instance SumOrProd DataTag 'True

type family DataTreeCols (meta :: TreeMeta) :: [[Type]] where
  DataTreeCols meta =
    GCode (TreeMetaType meta)

newtype DataPrim meta =
  DataPrim { unDataPrim :: TreeMetaType meta }

class GenDataTree (d :: Type) (tree :: Kind.Tree) | d -> tree where
  genDataTree :: d -> DataTree tree

instance (
    RootName d name,
    meta ~ 'TreeMeta ('NamedField name) Auto d,
    Tree DataParams d meta tree
  ) => GenDataTree d tree where
    genDataTree d =
      tree @DataParams @d @meta d

instance TreePrim DataTag Identity d name d where
  treePrim =
    pure

instance (
    ConstructSOP a ass
  ) => TreePrim DataTag Identity a ('NamedField "sum_index") Int where
  treePrim =
    Identity . hindex . gfrom

-- instance TreeProduct DataTag metas (NP I ds) ds where
--     treeProduct =
--       id

instance (
    ConstructSOP d '[ds]
  ) => TreeProduct DataTag metas d ds where
    treeProduct =
      unZ . unSOP . gfrom

-- instance (
--     ConstructSOP d dss
--   ) => TreeCons DataTag d (Maybe (NS (NP I) dss)) where
--   treeCons =
--     Just . unSOP . gfrom

-- instance TreeConElem DataTag (Maybe (NS (NP I) (ds : dss))) (Maybe (NS (NP I) dss)) (Maybe (NP I ds)) where
--   treeConElem = \case
--     Just (Z d) -> (Nothing, Just d)
--     Just (S ds) -> (Just ds, Nothing)
--     Nothing -> (Nothing, Nothing)

instance TreeConPayload DataTag name () where
  treeConPayload =
    ()

instance TreeEffects DefaultEffects rep d effs => TreeEffects DataTag rep d effs where

dataTree ::
  ∀ (d :: Type) (tree :: Kind.Tree) .
  GenDataTree d tree =>
  d ->
  DataTree tree
dataTree =
  genDataTree

class ReifyDataProd (tree :: Kind.Tree) d where
  reifyDataProd :: DataTree tree -> I d

instance (
    ReifyDataTree tree d
  ) => ReifyDataProd tree d where
  reifyDataProd t =
    I (reifyDataTree t)

class ReifyDataSum (tree :: Kind.Tree) (ds :: [*]) where
  reifyDataSum :: DataTree tree -> NP I ds

instance (
    AllZip ReifyDataProd node ds
  ) => ReifyDataSum ('Kind.Tree name eff ('Kind.Prod d node)) ds where
  reifyDataSum (Type.Tree _ (Type.Prod sub)) =
    htrans (Proxy @ReifyDataProd) reifyDataProd sub

class ReifyDataTree (tree :: Kind.Tree) d | tree -> d where
  reifyDataTree :: DataTree tree -> d

instance ReifyDataTree ('Kind.Tree name eff ('Kind.Prim d)) d where
  reifyDataTree (Type.Tree _ (Type.Prim (Identity d))) =
    d

instance (
    ReifySOP d '[ds],
    AllZip ReifyDataProd node ds
  ) => ReifyDataTree ('Kind.Tree name eff ('Kind.Prod d node)) d where
  reifyDataTree (Type.Tree _ (Type.Prod sub)) =
    gto (SOP (Z (htrans (Proxy @ReifyDataProd) reifyDataProd sub)))

instance (
    ReifySOP d dss,
    All Top node,
    AllZip ReifyDataSum node dss
  ) => ReifyDataTree ('Kind.Tree name eff ('Kind.Sum d node)) d where
  reifyDataTree (Type.Tree _ (Type.Sum sub)) =
    gto (SOP (htrans (Proxy @ReifyDataSum) reifyDataSum sub))
