module Polysemy.Db.Tree where

import Generics.SOP (NP(Nil, (:*)))

import Polysemy.Db.Data.Column (Auto, Con, Product, Rep)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import Polysemy.Db.Data.Uid (Uid)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Constraint (DataName, symbolText)
import Polysemy.Db.Text.DbIdentifier (quotedDbId)
import Polysemy.Db.Tree.Data.Effect (ADT)
import Polysemy.Db.Tree.Effect (ResolveTreeEffects)
import Polysemy.Db.Tree.Meta (ADTMetadata (ADTProd, ADTSum), ConMeta(ConMeta), TreeMeta(TreeMeta))
import qualified Polysemy.Db.Type.Data.Tree as Type

data Params =
  Params {
    tag :: Type,
    treeP :: Type,
    nodeP :: Type -> Type
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

class TreePrim (tag :: Type) (n :: Type -> Type) a (name :: FieldId) (d :: Type) where
  treePrim :: a -> n d

class TreePayload (tag :: Type) (t :: Type) a (meta :: TreeMeta) where
  treePayload :: a -> t

class TreeProduct (tag :: Type) d prod | tag d -> prod where
  treeProduct :: d -> prod

class TreeProductElem (tag :: Type) prod prodTail (meta :: TreeMeta) a | tag prod meta -> prodTail a where
  treeProductElem :: prod -> (prodTail, a)

class TreeConPayload (tag :: Type) (field :: FieldId) (t :: Type) where
  treeConPayload :: t

class TreeCons (tag :: Type) d cons | tag d -> cons where
  treeCons :: d -> cons

class TreeConElem (tag :: Type) cons consTail con | tag cons -> consTail con where
  treeConElem :: cons -> (consTail, con)

------------------------------------------------------------------------------------------------------------------------

class ProdTrees (p :: Params) (prod :: Type) (metas :: [TreeMeta]) (trees :: [Kind.Tree]) | p prod metas -> trees where
  prodTrees :: prod -> NP (TTree p) trees

instance ProdTrees p prod '[] '[] where
  prodTrees _ =
    Nil

-- TODO replace with hcmap or so
instance (
    meta ~ 'TreeMeta name rep a,
    TreeProductElem (Tag p) prod prodTail meta d,
    Tree p d meta tree,
    ProdTrees p prodTail metas trees
  ) => ProdTrees p prod (meta : metas) (tree : trees) where
    prodTrees p =
      tree @p @_ @meta a :* prodTrees @p @_ @metas pt
      where
        (pt, a) =
          treeProductElem @(Tag p) @_ @_ @meta p

------------------------------------------------------------------------------------------------------------------------

class ConTree (p :: Params) (con :: Type) (meta :: ConMeta) (tree :: Kind.Tree) | p con meta -> tree where
  conTree :: con -> TTree p tree

instance {-# overlappable #-} (
    p ~ 'Params tag t n,
    TreeConPayload tag field t,
    ProdTrees p con trees cs,
    c ~ 'Kind.Tree field '[] ('Kind.Prod (Con field) cs)
  ) => ConTree p con ('ConMeta field trees) c where
    conTree con =
      Type.Tree (treeConPayload @tag @field) (Type.Prod (prodTrees @p @_ @trees con))

instance (
    meta ~ 'TreeMeta cname rep d,
    Tree p con meta c
  ) => ConTree p con ('ConMeta cname '[ 'TreeMeta name rep d]) c where
    conTree =
      tree @p @_ @meta

------------------------------------------------------------------------------------------------------------------------

class SumTrees (p :: Params) (cons :: Type) (meta :: [ConMeta]) (trees :: [Kind.Tree]) | p cons meta -> trees where
  sumTrees :: cons -> NP (TTree p) trees

instance SumTrees p cons '[] '[] where
  sumTrees _ =
    Nil

instance (
    TreeConElem (Tag p) consData consTail conData,
    ConTree p conData conMeta c,
    SumTrees p consTail conMetas cs
  ) => SumTrees p consData (conMeta : conMetas) (c : cs) where
    sumTrees cons =
      conTree @p @_ @conMeta con :* sumTrees @p @_ @conMetas conTail
      where
        (conTail, con) =
          treeConElem @(Tag p) cons

------------------------------------------------------------------------------------------------------------------------

class AdtTree (p :: Params) (d :: Type) (a :: Type) (meta :: ADTMetadata) (eff :: [Type]) (node :: Kind.Node) | p d a meta eff -> node where
  adtTree :: a -> TNode p node

instance (
    TreeProduct tag a prod,
    ProdTrees p prod trees cs,
    p ~ 'Params tag t n
  ) => AdtTree p d a ('ADTProd trees) '[] ('Kind.Prod d cs) where
  adtTree a =
    Type.Prod (prodTrees @p @_ @trees (treeProduct @tag a))

type SumIndexTree e =
  'Kind.Tree ('NamedField "sum_index") e ('Kind.Prim Int)

instance (
    TreeCons tag a cons,
    SumTrees p cons trees cs,
    node ~ 'Kind.Sum d cs,
    p ~ 'Params tag t n
  ) => AdtTree p d a ('ADTSum trees) '[] node where
  adtTree a =
    Type.Sum (sumTrees @p @_ @trees (treeCons @tag a))

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
  node fa =
    Type.Prim (treePrim @tag @_ @_ @name @d fa)

instance {-# overlappable #-} (
    Node p name d a effs node
  ) => Node p name d a (eff : effs) node where
    node =
      node @p @name @d @_ @effs

------------------------------------------------------------------------------------------------------------------------

class Tree (p :: Params) (a :: Type) (meta :: TreeMeta) (tree :: Kind.Tree) | p a meta -> tree where
  tree :: a -> TTree p tree

instance (
    TreePayload tag t a meta,
    ResolveTreeEffects rep d effs,
    meta ~ 'TreeMeta name rep d,
    Node p name d a effs node,
    p ~ 'Params tag t n
  ) => Tree p a meta ('Kind.Tree name effs node) where
    tree a =
      Type.Tree (treePayload @tag @_ @_ @meta a) (node @p @name @d @_ @effs a)

------------------------------------------------------------------------------------------------------------------------

class KnownSymbol name => TableName (d :: Type) (name :: Symbol) | d -> name where
  tableName :: Text
  tableName =
    quotedDbId (symbolText @name)

instance {-# overlappable #-} (
    KnownSymbol name,
    DataName d name
  ) => TableName d name

instance TableName d name => TableName (Uid i d) name where

------------------------------------------------------------------------------------------------------------------------

class TableMeta (d :: Type) (meta :: TreeMeta) | d -> meta

instance {-# overlappable #-} (
    TableName d name
  ) => TableMeta d ('TreeMeta ('NamedField name) (Rep '[Product Auto]) d)

------------------------------------------------------------------------------------------------------------------------

class Root (p :: Params) (d :: Type) (a :: Type) (tree :: Kind.Tree) | p d a -> tree where
  root :: a -> TTree p tree

instance (
    TableMeta d meta,
    Tree p a meta c
  ) => Root p d a c where
  root =
    tree @p @a @meta
