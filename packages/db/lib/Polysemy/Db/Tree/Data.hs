module Polysemy.Db.Tree.Data where

import Generics.SOP (
  All,
  AllZip,
  I(I),
  NP ((:*)),
  NS (Z),
  POP(POP),
  SOP(SOP),
  Top,
  hexpand,
  hindex,
  hmap,
  htrans,
  unI,
  unSOP,
  unZ,
  )
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
  TreeConElem(..),
  TreeConPayload(..),
  TreeConProduct(..),
  TreeCons(..),
  TreePrim(..),
  TreeProduct(..),
  )
import Polysemy.Db.Tree.Effect (DefaultEffects, TreeEffects)
import Polysemy.Db.Tree.Meta (ConsTreeMeta(ConsTreeMeta), TreeMeta(TreeMeta), TreeMetaType)
import qualified Polysemy.Db.Type.Data.Tree as Type

data DataTag =
  DataTag
  deriving (Eq, Show)

data ExpandedDataTag =
  ExpandedDataTag
  deriving (Eq, Show)

type DataTree = Type.Tree () Identity
type DataNode = Type.Node () Identity
type DataParams = 'Params DataTag () Identity
type ExpandedDataParams = 'Params ExpandedDataTag () Maybe

instance SumOrProd DataTag 'True
instance SumOrProd ExpandedDataTag 'False

type family DataTreeCols (meta :: TreeMeta) :: [[Type]] where
  DataTreeCols meta =
    GCode (TreeMetaType meta)

class GenDataTree (d :: Type) (tree :: Kind.Tree) | d -> tree where
  genDataTree :: d -> DataTree tree

instance (
    RootName d name,
    meta ~ 'TreeMeta ('NamedField name) Auto d,
    Tree DataParams ('ConsTreeMeta d meta) tree
  ) => GenDataTree d tree where
    genDataTree d =
      tree @DataParams @('ConsTreeMeta d meta) d

instance TreePrim DataTag Identity d name d where
  treePrim =
    pure

instance (
    ConstructSOP a ass
  ) => TreePrim DataTag Identity a ('NamedField "sum_index") Int where
  treePrim =
    Identity . hindex . gfrom

instance TreePrim ExpandedDataTag Maybe (Maybe d) name d where
  treePrim =
    id

instance TreePrim ExpandedDataTag Maybe d name d where
  treePrim =
    Just

instance (
    ConstructSOP a ass
  ) => TreePrim ExpandedDataTag Maybe a ('NamedField "sum_index") Int where
  treePrim =
    Just . hindex . gfrom

instance (
    ConstructSOP d '[ds]
  ) => TreeProduct DataTag metas d ds where
    treeProduct =
      unZ . unSOP . gfrom

instance (
    ConstructSOP d dss
  ) => TreeCons ExpandedDataTag d (POP Maybe dss) where
  treeCons =
    hexpand Nothing . hmap (Just . unI) . gfrom

instance TreeConPayload DataTag name () where
  treeConPayload =
    ()

instance TreeConPayload ExpandedDataTag name () where
  treeConPayload =
    ()

instance TreeConElem ExpandedDataTag (POP Maybe (ds : dss)) (POP Maybe dss) (NP Maybe ds) where
  treeConElem (POP (ds :* dss)) =
    (POP dss, ds)

type family NPMaybes (ds :: [*]) :: [*] where
  NPMaybes '[] = '[]
  NPMaybes (d : ds) = Maybe d : NPMaybes ds

class AsMaybe x y where
  asMaybe :: Maybe x -> I y

instance AsMaybe x (Maybe x) where
  asMaybe = I

instance (
    AllZip AsMaybe ds ms,
    ms ~ NPMaybes ds
  ) => TreeConProduct ExpandedDataTag metas (NP Maybe ds) ms where
  treeConProduct =
    htrans (Proxy @AsMaybe) asMaybe

instance TreeEffects DefaultEffects rep d effs => TreeEffects DataTag rep d effs where

instance TreeEffects DefaultEffects rep d effs => TreeEffects ExpandedDataTag rep d effs where

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
  reifyDataSum (Type.Tree _ (Type.Prod _ sub)) =
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
  reifyDataTree (Type.Tree _ (Type.Prod _ sub)) =
    gto (SOP (Z (htrans (Proxy @ReifyDataProd) reifyDataProd sub)))

instance (
    ReifySOP d dss,
    All Top node,
    AllZip ReifyDataSum node dss
  ) => ReifyDataTree ('Kind.Tree name eff ('Kind.Sum d node)) d where
  reifyDataTree (Type.Tree _ (Type.Sum _ sub)) =
    gto (SOP (htrans (Proxy @ReifyDataSum) reifyDataSum sub))
