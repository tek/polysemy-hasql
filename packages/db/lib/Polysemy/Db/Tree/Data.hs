module Polysemy.Db.Tree.Data where

import Generics.SOP (AllZipN, I(I), NP ((:*)), NS (Z), SOP(SOP), htrans, unSOP, unZ)
import Generics.SOP.GGP (GCode, gfrom, gto)

import Polysemy.Db.Data.Column (Auto, Product, Rep)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Constraint (ConstructSOP, ReifySOP)
import Polysemy.Db.Tree (
  Params(Params),
  TableName,
  Tree(..),
  TreePayload(..),
  TreePrim(..),
  TreeProduct(..),
  TreeProductElem(..),
  )
import Polysemy.Db.Tree.Meta (TreeMetaType, TreeMeta(TreeMeta))
import qualified Polysemy.Db.Type.Data.Tree as Type

data DataTag =
  DataTag
  deriving (Eq, Show)

type DataTree = Type.Tree () Identity
type DataNode = Type.Node () Identity

type family DataTreeCols (meta :: TreeMeta) :: [[*]] where
  DataTreeCols meta =
    GCode (TreeMetaType meta)

newtype DataPrim meta =
  DataPrim { unDataPrim :: TreeMetaType meta }

class GenDataTree (d :: Type) (tree :: Kind.Tree) | d -> tree where
  genDataTree :: d -> DataTree tree

instance (
    TableName d name,
    meta ~ 'TreeMeta ('NamedField name) (Rep '[Product Auto]) d,
    params ~ 'Params DataTag () Identity,
    Tree params d meta tree
  ) => GenDataTree d tree where
    genDataTree d =
      tree @params @d @meta d

newtype ColMetaData meta =
  ColMetaData { unColMetaData :: TreeMetaType meta }

instance (
    a ~ TreeMetaType meta
  ) => TreePrim DataTag Identity a meta where
    treePrim =
      pure

instance TreePayload DataTag () a meta where
  treePayload _ =
    ()

instance (
    ConstructSOP d '[ds]
  ) => TreeProduct DataTag d (NP I ds) where
    treeProduct =
      unZ . unSOP . gfrom

instance TreeProductElem DataTag (NP I (d : ds)) (NP I ds) ('TreeMeta x y d) d where
  treeProductElem (I d :* ds) =
    (ds, d)

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
