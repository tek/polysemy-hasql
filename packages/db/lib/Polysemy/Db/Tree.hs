module Polysemy.Db.Tree where

import Generics.SOP (HTrans (htrans), NP (Nil, (:*)), NS (S, Z), POP (POP), SOP, unSOP)

import Polysemy.Db.Data.Column (NewtypeQuery, Prim, PrimQuery, Rep)
import Polysemy.Db.Data.FieldId (FieldId (NamedField))
import Polysemy.Db.Data.Uid (Uid)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Constraint (DataName, NewtypeCoded, symbolText)
import Polysemy.Db.SOP.Fundeps (Fundep, Fundeps)
import Polysemy.Db.Text.DbIdentifier (quotedDbId)
import Polysemy.Db.Tree.Api (TreePayload (..), TreePrim (..), TreeSOP (..))
import Polysemy.Db.Tree.Data.Effect (ADT, Newtype)
import Polysemy.Db.Tree.Data.Params (NodeParam, Params (Params), PayloadM, TCon, TNode, TTree)
import Polysemy.Db.Tree.Data.TreeMeta (ConMeta (ConMeta), ConMetaTypes, ConMetas, ConsMetas, TM (TM), TreeMeta (TreeMeta))
import Polysemy.Db.Tree.Effect (TreeEffects)
import Polysemy.Db.Tree.Meta (AdtMetadata (AdtProd, AdtSum), ProdDefaultRep)
import qualified Polysemy.Db.Type.Data.Tree as Type

class ProdTrees (p :: Params) (metas :: [TreeMeta]) (trees :: [Kind.Tree]) | p metas -> trees where
  prodTrees :: NP (PayloadM p) metas -> NP (TTree p) trees

instance (
    Fundeps (Fundep (Tree p)) metas trees
  ) => ProdTrees p metas trees where
    prodTrees =
      htrans (Proxy @(Tree p)) (tree @p)

------------------------------------------------------------------------------------------------------------------------

class ConTree (p :: Params) (meta :: ConMeta) (con :: Kind.Con) | p meta -> con where
  conTree :: NP (PayloadM p) (ConMetas meta) -> TCon p con

instance (
    p ~ 'Params tag t n sfp,
    ProdTrees p (meta0 : meta1 : metas) trees
  ) => ConTree p ('ConMeta num name (meta0 : meta1 : metas)) ('Kind.Con num name trees) where
    conTree con =
      Type.Con (prodTrees @p con)

instance (
    p ~ 'Params tag t n sfp,
    Tree p meta tree
  ) => ConTree p ('ConMeta num name '[meta]) ('Kind.ConUna num name tree) where
    conTree (payload :* Nil) =
      Type.ConUna (tree @p @meta payload)

------------------------------------------------------------------------------------------------------------------------

class SumProdTrees (p :: Params) (metas :: [ConMeta]) (trees :: [Kind.Con]) | p metas -> trees where
  sumProdTrees :: POP (PayloadM p) (ConsMetas metas) -> NP (TCon p) trees

instance SumProdTrees p '[] '[] where
  sumProdTrees _ =
    Nil

instance (
    ConTree p meta tree,
    SumProdTrees p metas trees
  ) => SumProdTrees p (meta : metas) (tree : trees) where
    sumProdTrees (POP (con :* cons)) =
      conTree @p @meta con :* sumProdTrees @p @metas (POP cons)

------------------------------------------------------------------------------------------------------------------------

class SumTrees (p :: Params) (metas :: [ConMeta]) (trees :: [Kind.Con]) | p metas -> trees where
  sumTrees :: NS (NP (PayloadM p)) (ConsMetas metas) -> NS (TCon p) trees

instance SumTrees p '[] '[] where
  sumTrees _ =
    undefined

instance (
    ds ~ ConMetaTypes meta,
    ConTree p meta tree,
    SumTrees p metas trees
  ) => SumTrees p (meta : metas) (tree : trees) where
  sumTrees = \case
    Z con -> Z (conTree @p @meta @tree con)
    S cons -> S (sumTrees @p @metas @trees cons)

------------------------------------------------------------------------------------------------------------------------

class SumNode (p :: Params) (d :: Type) (metas :: [ConMeta]) (node :: Kind.Node) | p d metas -> node where
  sumNode :: NodeParam p d -> TNode p node

instance (
    p ~ 'Params tag t n 'True,
    metas ~ ConsMetas conMetas,
    sumName ~ 'NamedField "sum",
    TreeSOP tag metas POP n d,
    TreePrim tag n sumName d,
    SumProdTrees p conMetas trees
  ) => SumNode ('Params tag t n 'True) d conMetas ('Kind.SumProd d trees) where
  sumNode fd =
    Type.SumProd (treePrim @tag @n @sumName @d fd) trees
    where
      trees =
        sumProdTrees @p @conMetas (treeSOP @_ @tag @metas fd)

instance (
    p ~ 'Params tag t n 'False,
    metas ~ ConsMetas conMetas,
    sumName ~ 'NamedField "sum",
    TreeSOP tag metas SOP n d,
    TreePrim tag n sumName d,
    SumTrees p conMetas trees
  ) => SumNode ('Params tag t n 'False) d conMetas ('Kind.Sum d trees) where
    sumNode fd =
      Type.Sum (treePrim @tag @n @sumName @d fd) (sumTrees @p @conMetas (unSOP (treeSOP @_ @tag @metas fd)))

------------------------------------------------------------------------------------------------------------------------

class AdtNode (p :: Params) (d :: Type) (meta :: AdtMetadata) (eff :: [Type]) (node :: Kind.Node) | p d meta eff -> node where
  adtTree :: NodeParam p d -> TNode p node

instance (
    TreeSOP tag metas NP n d,
    prodName ~ 'NamedField "prod",
    TreePrim tag n prodName d,
    ProdTrees p metas trees,
    p ~ 'Params tag t n sfp
  ) => AdtNode p d ('AdtProd metas) '[] ('Kind.Prod d trees) where
  adtTree fd =
    Type.Prod (treePrim @tag @n @prodName @d fd) (prodTrees @p @metas (treeSOP @TreeMeta @tag @metas fd))

instance (
    p ~ 'Params tag t n sfp,
    SumNode p d metas node
  ) => AdtNode p d ('AdtSum metas) '[] node where
  adtTree =
    sumNode @p @d @metas

------------------------------------------------------------------------------------------------------------------------

class Node (p :: Params) (name :: FieldId) (d :: Type) (eff :: [Type]) (node :: Kind.Node) | p name d eff -> node where
  node :: NodeParam p d -> TNode p node

instance (
    AdtNode p d meta effs node
  ) => Node p name d (ADT meta rep : effs) node where
    node =
      adtTree @p @d @meta @effs

instance (
    TreePrim tag n name d
  ) => Node ('Params tag t n sfp) name d '[] ('Kind.Prim d) where
  node a =
    Type.Prim (treePrim @tag @n @name @d a)

instance {-# overlappable #-} (
    Node p name d effs node
  ) => Node p name d (eff : effs) node where
    node =
      node @p @name @d @effs

------------------------------------------------------------------------------------------------------------------------

class Tree (p :: Params) (meta :: TreeMeta) (tree :: Kind.Tree) | p meta -> tree where
  tree :: PayloadM p meta -> TTree p tree

instance (
    TreeEffects tag rep d effs,
    TreePayload tag meta effs t,
    meta ~ 'TreeMeta name rep d,
    Node p name d effs node,
    p ~ 'Params tag t n sfp
  ) => Tree p meta ('Kind.Tree name effs node) where
    tree (TM a) =
      Type.Tree (treePayload @tag @meta @effs) (node @p @name @d @effs a)

instance Tree p meta tree => Fundep (Tree p) meta tree where

------------------------------------------------------------------------------------------------------------------------

class KnownSymbol name => RootName (d :: Type) (name :: Symbol) | d -> name where
  tableName :: Text
  tableName =
    quotedDbId (symbolText @name)

instance {-# overlappable #-} (
    KnownSymbol name,
    DataName d name
  ) => RootName d name

instance RootName d name => RootName (Uid i d) name where

------------------------------------------------------------------------------------------------------------------------

class RootMeta (rep :: Type) (d :: Type) (meta :: TreeMeta) | rep d -> meta

instance {-# overlappable #-} (
    RootName d name,
    meta ~ 'TreeMeta ('NamedField name) (ProdDefaultRep rep) d
  ) => RootMeta rep d meta

instance RootMeta (PrimQuery name) d ('TreeMeta ('NamedField name) (Rep '[Prim]) d)

instance (
    NewtypeCoded nt d
  ) => RootMeta (NewtypeQuery name) nt ('TreeMeta ('NamedField name) (Rep '[Newtype nt d, Prim]) nt)

------------------------------------------------------------------------------------------------------------------------

class Root (rep :: Type) (p :: Params) (d :: Type) (tree :: Kind.Tree) | rep p d -> tree where
  root :: NodeParam p d -> TTree p tree

instance (
    meta ~ 'TreeMeta name r d,
    RootMeta rep d meta,
    Tree p meta tree
  ) => Root rep p d tree where
  root =
    tree @p @meta . TM
