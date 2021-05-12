module Polysemy.Db.Tree where

import Generics.SOP (HTrans (htrans), I(I), K(K), NP(Nil, (:*)), NS (Z, S), POP(POP), unSOP)

import Polysemy.Db.Data.Column (Con(Con), Prim, PrimQuery, Rep)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import Polysemy.Db.Data.Uid (Uid)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Constraint (DataName, symbolText)
import Polysemy.Db.SOP.Fundeps (Fundep, Fundeps)
import Polysemy.Db.Text.DbIdentifier (quotedDbId)
import Polysemy.Db.Tree.Api (
  TreeConPayload(..),
  TreePayload(..),
  TreePrim(..),
  TreeProduct(..),
  TreeSum(..),
  TreeSumProd(..),
  )
import Polysemy.Db.Tree.Data.Effect (ADT)
import Polysemy.Db.Tree.Data.Params (Params(Params), Payload, PayloadM, TNode, TTree)
import Polysemy.Db.Tree.Data.TreeMeta (
  ConMeta(ConMeta),
  ConMetaTypes,
  ConMetas,
  ConsMetas,
  TM(TM),
  TreeMeta(TreeMeta),
  TreeMetaTypes,
  )
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

class ConTree (p :: Params) (meta :: ConMeta) (tree :: Kind.Tree) | p meta -> tree where
  conTree :: NP (PayloadM p) (ConMetas meta) -> TTree p tree

instance {-# overlappable #-} (
    Applicative n,
    p ~ 'Params tag t n f,
    TreeConPayload tag field t,
    ProdTrees p metas trees,
    tree ~ 'Kind.Tree field '[] ('Kind.Prod (Con field) trees)
  ) => ConTree p ('ConMeta field metas) tree where
    conTree con =
      Type.Tree (treeConPayload @tag @field) (Type.Prod (pure Con) (prodTrees @p @metas con))

instance (
    meta ~ 'TreeMeta name rep d,
    con ~ Payload p d,
    Tree p meta tree
  ) => ConTree p ('ConMeta cname '[ 'TreeMeta name rep d]) tree where
    conTree (payload :* Nil) =
      tree @p @meta payload

------------------------------------------------------------------------------------------------------------------------

class SumProdTrees (p :: Params) (metas :: [ConMeta]) (trees :: [Kind.Tree]) | p metas -> trees where
  sumProdTrees :: POP (PayloadM p) (ConsMetas metas) -> NP (TTree p) trees

instance SumProdTrees p '[] '[] where
  sumProdTrees _ =
    Nil

-- TODO map
instance (
    ConTree p meta tree,
    SumProdTrees p metas trees
  ) => SumProdTrees p (meta : metas) (tree : trees) where
    sumProdTrees (POP (con :* cons)) =
      conTree @p @meta con :* sumProdTrees @p @metas (POP cons)

------------------------------------------------------------------------------------------------------------------------

class SumConTree (p :: Params) (meta :: ConMeta) (tree :: Kind.Tree) | p meta -> tree where
  sumConTree :: NP (PayloadM p) (ConMetas meta) -> TTree p tree

instance (
    Applicative n,
    p ~ 'Params tag t n I,
    prod ~ TreeMetaTypes nodes,
    TreeConPayload tag field t,
    ProdTrees p nodes node,
    tree ~ 'Kind.Tree field '[] ('Kind.Prod (Con field) node)
  ) => SumConTree p ('ConMeta field nodes) tree where
    sumConTree con =
      Type.Tree (treeConPayload @tag @field) (Type.Prod (pure Con) (prodTrees @p @nodes con))

------------------------------------------------------------------------------------------------------------------------

class SumTrees (p :: Params) (metas :: [ConMeta]) (trees :: [Kind.Tree]) | p metas -> trees where
  sumTrees :: NS (NP (PayloadM p)) (ConsMetas metas) -> NS (TTree p) trees

instance SumTrees p '[] '[] where
  sumTrees _ =
    undefined

instance (
    ds ~ ConMetaTypes meta,
    SumConTree p meta tree,
    SumTrees p metas trees
  ) => SumTrees p (meta : metas) (tree : trees) where
  sumTrees = \case
    Z con -> Z (sumConTree @p @meta @tree con)
    S cons -> S (sumTrees @p @metas @trees cons)

------------------------------------------------------------------------------------------------------------------------

class ProdForSumTree (tag :: Type) (decision :: Bool) | tag -> decision where

class SumNode (p :: Params) (sum :: Bool) (d :: Type) (metas :: [ConMeta]) (node :: Kind.Node) | p sum d metas -> node where
  sumNode :: Payload p d -> TNode p node

type SumIndex =
  'Kind.Tree ('NamedField "sum_index") '[Prim] ('Kind.Prim Int)

instance (
    p ~ 'Params tag t n f,
    sumName ~ 'NamedField "sum",
    indexMeta ~ 'TreeMeta ('NamedField "sum_index") Prim Int,
    indexParams ~ 'Params tag t n (K ()),
    TreeSumProd tag f metas d,
    TreePrim tag n (f d) sumName d,
    SumProdTrees p metas trees,
    Tree indexParams indexMeta SumIndex
  ) => SumNode p 'True d metas ('Kind.Prod d (SumIndex : trees)) where
  sumNode fd =
    Type.Prod (treePrim @tag @n @(f d) @sumName @d fd) trees
    where
      trees =
        indexTree :* sumProdTrees @p @metas (treeSumProd @tag @f @metas fd)
      indexTree =
        tree @indexParams @indexMeta (TM (K ()))

instance (
    p ~ 'Params tag t n I,
    sumName ~ 'NamedField "sum",
    TreeSum tag I metas d,
    TreePrim tag n (I d) sumName d,
    SumTrees p metas trees
  ) => SumNode p 'False d metas ('Kind.Sum d trees) where
    sumNode (I d) =
      Type.Sum (treePrim @tag @n @(I d) @sumName @d (I d)) (sumTrees @p @metas (unSOP (treeSum @tag @I @metas @d (I d))))

------------------------------------------------------------------------------------------------------------------------

class AdtTree (p :: Params) (d :: Type) (meta :: AdtMetadata) (eff :: [Type]) (node :: Kind.Node) | p d meta eff -> node where
  adtTree :: Payload p d -> TNode p node

instance (
    TreeProduct tag f metas (f d),
    prodName ~ 'NamedField "prod",
    TreePrim tag n (f d) prodName d,
    ProdTrees p metas trees,
    p ~ 'Params tag t n f
  ) => AdtTree p d ('AdtProd metas) '[] ('Kind.Prod d trees) where
  adtTree fd =
    Type.Prod (treePrim @tag @n @(f d) @prodName @d fd) (prodTrees @p @metas (treeProduct @tag @f @metas fd))

instance (
    p ~ 'Params tag t n f,
    ProdForSumTree tag sum,
    SumNode p sum d metas node
  ) => AdtTree p d ('AdtSum metas) '[] node where
  adtTree =
    sumNode @p @sum @d @metas

------------------------------------------------------------------------------------------------------------------------

class Node (p :: Params) (name :: FieldId) (d :: Type) (eff :: [Type]) (node :: Kind.Node) | p name d eff -> node where
  node :: Payload p d -> TNode p node

instance (
    AdtTree p d meta effs node
  ) => Node p name d (ADT meta rep : effs) node where
    node =
      adtTree @p @d @meta @effs

instance (
    TreePrim tag n (f d) name d
  ) => Node ('Params tag t n f) name d '[] ('Kind.Prim d) where
  node a =
    Type.Prim (treePrim @tag @_ @_ @name @d a)

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
    TreePayload tag t (f d) meta effs,
    meta ~ 'TreeMeta name rep d,
    Node p name d effs node,
    p ~ 'Params tag t n f
  ) => Tree p meta ('Kind.Tree name effs node) where
    tree (TM a) =
      Type.Tree (treePayload @tag @_ @_ @meta @effs a) (node @p @name @d @effs a)

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

------------------------------------------------------------------------------------------------------------------------

class Root (rep :: Type) (p :: Params) (d :: Type) (tree :: Kind.Tree) | rep p d -> tree where
  root :: Payload p d -> TTree p tree

instance (
    meta ~ 'TreeMeta name r d,
    RootMeta rep d meta,
    Tree p meta tree
  ) => Root rep p d tree where
  root =
    tree @p @meta . TM
