module Polysemy.Db.Tree.Data where

import Generics.SOP (AllZip, I(I), NP, NS (Z), SListI, SOP(SOP), htrans)
import Generics.SOP.GGP (GCode, gto)

import Polysemy.Db.Data.Column (Auto)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Constraint (ReifySOP)
import Polysemy.Db.Tree (
  ProdForSumTree,
  RootName,
  Tree(..),
  )
import Polysemy.Db.Tree.Data.Params (Params(Params))
import Polysemy.Db.Tree.Data.TreeMeta (TM(TM), TreeMeta(TreeMeta), TreeMetaType)
import Polysemy.Db.Tree.Effect (DefaultEffects, TreeEffects)
import qualified Polysemy.Db.Type.Data.Tree as Type

data DataTag =
  DataTag
  deriving (Eq, Show)

data ExpandedDataTag =
  ExpandedDataTag
  deriving (Eq, Show)

type DataTree = Type.Tree () I
type DataNode = Type.Node () I
type DataParams = 'Params DataTag () I
type ExpandedDataParams = 'Params ExpandedDataTag () Maybe

instance ProdForSumTree DataTag 'False
instance ProdForSumTree ExpandedDataTag 'True

type family DataTreeCols (meta :: TreeMeta) :: [[Type]] where
  DataTreeCols meta =
    GCode (TreeMetaType meta)

class GenDataTree (d :: Type) (tree :: Kind.Tree) | d -> tree where
  genDataTree :: d -> DataTree tree

instance (
    RootName d name,
    meta ~ 'TreeMeta ('NamedField name) Auto d,
    Tree DataParams meta tree
  ) => GenDataTree d tree where
    genDataTree d =
      tree @DataParams @meta (TM (pure d))

instance TreeEffects DefaultEffects rep d effs => TreeEffects DataTag rep d effs where

type family NPMaybes (ds :: [*]) :: [*] where
  NPMaybes '[] = '[]
  NPMaybes (d : ds) = Maybe d : NPMaybes ds

class AsMaybe x y where
  asMaybe :: Maybe x -> I y

instance AsMaybe x (Maybe x) where
  asMaybe = I

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
  reifyDataTree (Type.Tree _ (Type.Prim (I d))) =
    d

instance (
    ReifySOP d '[ds],
    AllZip ReifyDataProd node ds
  ) => ReifyDataTree ('Kind.Tree name eff ('Kind.Prod d node)) d where
  reifyDataTree (Type.Tree _ (Type.Prod _ sub)) =
    gto (SOP (Z (htrans (Proxy @ReifyDataProd) reifyDataProd sub)))

instance (
    ReifySOP d dss,
    SListI node,
    AllZip ReifyDataSum node dss
  ) => ReifyDataTree ('Kind.Tree name eff ('Kind.Sum d node)) d where
  reifyDataTree (Type.Tree _ (Type.Sum _ sub)) =
    gto (SOP (htrans (Proxy @ReifyDataSum) reifyDataSum sub))
