module Polysemy.Db.Tree.Data where

import Generics.SOP (AllZipN, I(I), NP, NS (Z), SOP(SOP), htrans, unSOP, unZ)
import Generics.SOP.GGP (GCode, gfrom, gto)

import Polysemy.Db.Data.Column (Auto, Product, Rep)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Constraint (ConstructSOP, ReifySOP)
import Polysemy.Db.Tree (
  Params(Params),
  RootName,
  Tree(..),
  TreePayload(..),
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

type family DataTreeCols (meta :: TreeMeta) :: [[Type]] where
  DataTreeCols meta =
    GCode (TreeMetaType meta)

newtype DataPrim meta =
  DataPrim { unDataPrim :: TreeMetaType meta }

class GenDataTree (d :: Type) (tree :: Kind.Tree) | d -> tree where
  genDataTree :: d -> DataTree tree

instance (
    RootName d name,
    meta ~ 'TreeMeta ('NamedField name) (Rep '[Product Auto]) d,
    params ~ 'Params DataTag () Identity,
    Tree params d meta tree
  ) => GenDataTree d tree where
    genDataTree d =
      tree @params @d @meta d

instance TreePrim DataTag Identity d name d where
  treePrim =
    pure

instance TreePayload DataTag () a meta effs where
  treePayload _ =
    ()

instance (
    ConstructSOP d '[ds]
  ) => TreeProduct DataTag metas d ds where
    treeProduct =
      unZ . unSOP . gfrom

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

class ReifyDataTree (tree :: Kind.Tree) d | tree -> d where
  reifyDataTree :: DataTree tree -> d

instance ReifyDataTree ('Kind.Tree name eff ('Kind.Prim d)) d where
  reifyDataTree (Type.Tree _ (Type.Prim (Identity d))) =
    d

instance (
    ReifySOP d '[ds],
    AllZipN NP ReifyDataProd node ds
  ) => ReifyDataTree ('Kind.Tree name eff ('Kind.Prod d node)) d where
  reifyDataTree (Type.Tree _ (Type.Prod sub)) =
    gto (SOP (Z (htrans (Proxy @ReifyDataProd) reifyDataProd sub)))
