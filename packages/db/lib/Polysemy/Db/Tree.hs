module Polysemy.Db.Tree where

import Generics.SOP (I(I), NP(Nil, (:*)))

import Polysemy.Db.Data.Column (Con, Prim, PrimQuery, Rep)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import Polysemy.Db.Data.Uid (Uid)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Constraint (DataName, symbolText)
import Polysemy.Db.SOP.List (NPAsUnit (npAsUnit))
import Polysemy.Db.Text.DbIdentifier (quotedDbId)
import Polysemy.Db.Tree.Data.Effect (ADT)
import Polysemy.Db.Tree.Effect (TreeEffects)
import Polysemy.Db.Tree.Meta (ADTMetadata (ADTProd, ADTSum), ConMeta, ConMeta(ConMeta), ProdDefaultRep, TreeMeta(TreeMeta))
import qualified Polysemy.Db.Type.Data.Tree as Type

data Params =
  Params {
    tag :: Type,
    treeParam :: Type,
    nodeParam :: Type -> Type
  }

type family Tag (params :: Params) :: Type where
  Tag ('Params tag _ _) = tag

type family TreeParam (params :: Params) :: Type where
  TreeParam ('Params _ tree _) = tree

type family NodeParam (params :: Params) :: Type -> Type where
  NodeParam ('Params _ _ node) = node

type family TTree (params :: Params) :: Kind.Tree -> Type where
  TTree ('Params _ t n) = Type.Tree t n

type family TNode (params :: Params) :: Kind.Node -> Type where
  TNode ('Params _ t n) = Type.Node t n

class TreePrim (tag :: Type) (n :: Type -> Type) (a :: Type) (name :: FieldId) (d :: Type) where
  treePrim :: a -> n d

class TreePayload (tag :: Type) (t :: Type) (a :: Type) (meta :: TreeMeta) (effs :: [Type]) where
  treePayload :: a -> t

instance TreePayload tag () () meta effs where
  treePayload _ =
    ()

class TreeProduct (tag :: Type) (metas :: [TreeMeta]) (d :: Type) (prod :: [Type]) | tag metas d -> prod where
  treeProduct :: d -> NP I prod

instance (
    NPAsUnit metas ds
  ) => TreeProduct tag metas () ds where
  treeProduct _ =
    npAsUnit @_ @metas

class TreeConPayload (tag :: Type) (field :: FieldId) (t :: Type) where
  treeConPayload :: t

class TreeCons (tag :: Type) (d :: Type) (cons :: Type) | tag d -> cons where
  treeCons :: d -> cons

class TreeConElem (tag :: Type) (cons :: Type) (consTail :: Type) (con :: Type) | tag cons -> consTail con where
  treeConElem :: cons -> (consTail, con)

------------------------------------------------------------------------------------------------------------------------

data ProdMeta =
  ProdMeta {
    pmMeta :: TreeMeta,
    pmProd :: Type
  }

class ProdTrees (p :: Params) (prod :: [Type]) (metas :: [TreeMeta]) (trees :: [Kind.Tree]) | p prod metas -> trees where
  prodTrees :: NP I prod -> NP (TTree p) trees

instance ProdTrees p prod '[] '[] where
  prodTrees _ =
    Nil

instance (
    meta ~ 'TreeMeta name rep d,
    Tree p prodH meta tree,
    ProdTrees p prodTail metas trees
  ) => ProdTrees p (prodH : prodTail) (meta : metas) (tree : trees) where
    prodTrees (I prodH :* prodTail) =
      tree @p @_ @meta prodH :* prodTrees @p @_ @metas prodTail

------------------------------------------------------------------------------------------------------------------------

class ConTree (p :: Params) (con :: Type) (meta :: ConMeta) (tree :: Kind.Tree) | p con meta -> tree where
  conTree :: con -> TTree p tree

instance {-# overlappable #-} (
    p ~ 'Params tag t n,
    TreeConPayload tag field t,
    TreeProduct tag metas con prod,
    ProdTrees p prod metas node,
    tree ~ 'Kind.Tree field '[] ('Kind.Prod (Con field) node)
  ) => ConTree p con ('ConMeta field metas) tree where
    conTree con =
      Type.Tree (treeConPayload @tag @field) (Type.Prod (prodTrees @p @prod @metas (treeProduct @tag @metas @con con)))

instance (
    meta ~ 'TreeMeta cname rep d,
    Tree p con meta tree
  ) => ConTree p con ('ConMeta cname '[ 'TreeMeta name rep d]) tree where
    conTree =
      tree @p @_ @meta

------------------------------------------------------------------------------------------------------------------------

class SumTrees (p :: Params) (cons :: Type) (metas :: [ConMeta]) (trees :: [Kind.Tree]) | p cons metas -> trees where
  sumTrees :: cons -> NP (TTree p) trees

instance SumTrees p cons '[] '[] where
  sumTrees _ =
    Nil

instance (
    TreeConElem (Tag p) consData consTail conData,
    ConTree p conData meta tree,
    SumTrees p consTail metas trees
  ) => SumTrees p consData (meta : metas) (tree : trees) where
    sumTrees cons =
      conTree @p @_ @meta con :* sumTrees @p @_ @metas conTail
      where
        (conTail, con) =
          treeConElem @(Tag p) cons

------------------------------------------------------------------------------------------------------------------------

class AdtTree (p :: Params) (d :: Type) (a :: Type) (meta :: ADTMetadata) (eff :: [Type]) (node :: Kind.Node) | p d a meta eff -> node where
  adtTree :: a -> TNode p node

instance (
    TreeProduct tag metas a prod,
    ProdTrees p prod metas trees,
    p ~ 'Params tag t n
  ) => AdtTree p d a ('ADTProd metas) '[] ('Kind.Prod d trees) where
  adtTree a =
    Type.Prod (prodTrees @p @prod @metas (treeProduct @tag @metas a))

type SumIndexTree =
  'Kind.Tree ('NamedField "sum_index") '[Prim] ('Kind.Prim Int)

instance (
    TreeCons tag a cons,
    indexName ~ 'NamedField "sum_index",
    indexMeta ~ 'TreeMeta indexName Prim Int,
    TreePrim tag n a indexName Int,
    TreePayload tag t a indexMeta '[Prim],
    SumTrees p cons metas trees,
    node ~ 'Kind.Sum d (SumIndexTree : trees),
    p ~ 'Params tag t n
  ) => AdtTree p d a ('ADTSum metas) '[] node where
  adtTree a =
    Type.Sum (indexTree :* sumTrees @p @_ @metas (treeCons @tag a))
    where
      indexTree =
        Type.Tree indexPayload (Type.Prim (treePrim @tag @_ @_ @indexName @Int a))
      indexPayload =
        treePayload @tag @_ @_ @indexMeta @'[Prim] a

------------------------------------------------------------------------------------------------------------------------

class Node (p :: Params) (name :: FieldId) (d :: Type) (a :: Type) (eff :: [Type]) (node :: Kind.Node) | p name d a eff -> node where
  node :: a -> TNode p node

instance (
    AdtTree p d a meta effs node
  ) => Node p name d a (ADT meta rep : effs) node where
    node =
      adtTree @p @d @_ @meta @effs

instance (
    TreePrim tag n a name d
  ) => Node ('Params tag t n) name d a '[] ('Kind.Prim d) where
  node a =
    Type.Prim (treePrim @tag @_ @_ @name @d a)

instance {-# overlappable #-} (
    Node p name d a effs node
  ) => Node p name d a (eff : effs) node where
    node =
      node @p @name @d @_ @effs

------------------------------------------------------------------------------------------------------------------------

class Tree (p :: Params) (a :: Type) (meta :: TreeMeta) (tree :: Kind.Tree) | p a meta -> tree where
  tree :: a -> TTree p tree

instance (
    TreePayload tag t a meta effs,
    TreeEffects tag rep d effs,
    meta ~ 'TreeMeta name rep d,
    Node p name d a effs node,
    p ~ 'Params tag t n
  ) => Tree p a meta ('Kind.Tree name effs node) where
    tree a =
      Type.Tree (treePayload @tag @_ @_ @meta @effs a) (node @p @name @d @_ @effs a)

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

class Root (rep :: Type) (p :: Params) (d :: Type) (a :: Type) (tree :: Kind.Tree) | rep p d a -> tree where
  root :: a -> TTree p tree

instance (
    RootMeta rep d meta,
    Tree p a meta c
  ) => Root rep p d a c where
  root =
    tree @p @a @meta
