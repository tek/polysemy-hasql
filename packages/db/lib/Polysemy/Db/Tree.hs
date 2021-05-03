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
import Polysemy.Db.Tree.Meta (ADTMetadata (ADTProd, ADTSum), TreeMetaType, ConMeta(ConMeta), TreeMeta(TreeMeta))
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

type family TTree (params :: Params) :: Kind.Tree -> * where
  TTree ('Params _ t n) = Type.Tree t n

type family TNode (params :: Params) :: Kind.Node -> * where
  TNode ('Params _ t n) = Type.Node t n

class TreePrim (tag :: Type) (n :: Type -> Type) a (meta :: TreeMeta) where
  treePrim :: a -> n (TreeMetaType meta)

class TreePayload (tag :: Type) (t :: Type) a (meta :: TreeMeta) where
  treePayload :: a -> t

class TreeProduct (tag :: Type) d prod | tag d -> prod where
  treeProduct :: d -> prod

class TreeProductElem (tag :: Type) prod prodTail (meta :: TreeMeta) a | tag prod meta -> prodTail a where
  treeProductElem :: prod -> (prodTail, a)

class TreeCons (tag :: Type) d cons | tag d -> cons where
  treeCons :: d -> cons

class TreeConElem (tag :: Type) cons consTail con | tag cons -> consTail con where
  treeConElem :: cons -> (consTail, con)

class ProdTrees (p :: Params) (prod :: Type) (metas :: [TreeMeta]) (cs :: [Kind.Tree]) | p metas -> cs where
  prodTrees :: prod -> NP (TTree p) cs

instance ProdTrees p prod '[] '[] where
  prodTrees _ =
    Nil

-- TODO replace with hcmap or so
instance (
    meta ~ 'TreeMeta name rep a,
    TreeProductElem (Tag p) prod prodTail meta d,
    Tree p d meta c,
    ProdTrees p prodTail metas cs
  ) => ProdTrees p prod (meta : metas) (c : cs) where
    prodTrees p =
      tree @p @_ @meta a :* prodTrees @p @_ @metas pt
      where
        (pt, a) =
          treeProductElem @(Tag p) @_ @_ @meta p

class ConTree (p :: Params) (con :: Type) (meta :: ConMeta) (tree :: Kind.Tree) | p meta -> tree where
  conTree :: con -> TTree p tree

-- TODO undefined
instance {-# overlappable #-} (
    p ~ 'Params tag t n,
    TreePayload tag t a meta,
    ProdTrees p con trees cs,
    c ~ 'Kind.Tree name '[] ('Kind.Prod (Con name) cs)
  ) => ConTree p con ('ConMeta name trees) c where
    conTree con =
      Type.Tree (treePayload @tag @_ @a @meta undefined) (Type.Prod (prodTrees @p @_ @trees con))

instance (
    meta ~ 'TreeMeta cname rep d,
    Tree p con meta c
  ) => ConTree p con ('ConMeta cname '[ 'TreeMeta name rep d]) c where
    conTree =
      tree @p @_ @meta

class SumTrees (p :: Params) (cons :: Type) (meta :: [ConMeta]) (trees :: [Kind.Tree]) | p meta -> trees where
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

class AdtTree (p :: Params) (d :: Type) (meta :: ADTMetadata) (eff :: [*]) (node :: Kind.Node) | p d meta eff -> node where
  adtTree :: d -> TNode p node

instance (
    TreeProduct tag d prod,
    ProdTrees p prod trees cs,
    p ~ 'Params tag t n
  ) => AdtTree p d ('ADTProd trees) '[] ('Kind.Prod d cs) where
  adtTree d =
    Type.Prod (prodTrees @p @_ @trees (treeProduct @tag d))

type SumIndexTree e =
  'Kind.Tree ('NamedField "sum_index") e ('Kind.Prim Int)

instance (
    TreeCons tag d cons,
    SumTrees p cons trees cs,
    node ~ 'Kind.Sum d cs,
    p ~ 'Params tag t n
  ) => AdtTree p d ('ADTSum trees) '[] node where
  adtTree d =
    Type.Sum (sumTrees @p @_ @trees (treeCons @tag d))

class Node (p :: Params) (meta :: TreeMeta) (a :: *) (eff :: [*]) (d :: Type) (node :: Kind.Node) | p eff d -> node where
  node :: a -> TNode p node

instance (
    AdtTree p d meta effs node
  ) => Node p _meta d (ADT meta rep : effs) d node where
    node =
      adtTree @p @_ @meta @effs

instance (
    TreePrim tag n a meta,
    meta ~ 'TreeMeta name rep d
  ) => Node ('Params tag t n) meta a '[] d ('Kind.Prim d) where
  node fa =
    Type.Prim (treePrim @tag @_ @_ @meta fa)

instance {-# overlappable #-} (
    Node p meta a effs d node
  ) => Node p meta a (eff : effs) d node where
    node =
      node @p @meta @_ @effs @d

class Tree (p :: Params) (a :: Type) (meta :: TreeMeta) (tree :: Kind.Tree) | p meta -> tree where
  tree :: a -> TTree p tree

instance (
    TreePayload tag t a meta,
    ResolveTreeEffects rep d effs,
    meta ~ 'TreeMeta name rep d,
    Node p meta a effs d node,
    tree ~ 'Kind.Tree name '[] node,
    p ~ 'Params tag t n
  ) => Tree p a meta tree where
    tree a =
      Type.Tree (treePayload @tag @_ @_ @meta a) (node @p @meta @_ @effs @d a)

class KnownSymbol name => TableName (d :: Type) (name :: Symbol) | d -> name where
  tableName :: Text
  tableName =
    quotedDbId (symbolText @name)

instance {-# overlappable #-} (
    KnownSymbol name,
    DataName d name
  ) => TableName d name

instance TableName d name => TableName (Uid i d) name where

class TableMeta (d :: Type) (meta :: TreeMeta) | d -> meta

instance {-# overlappable #-} (
    TableName d name,
    meta ~ 'TreeMeta ('NamedField name) (Rep '[Product Auto]) d
  ) => TableMeta d meta

class Root (p :: Params) (d :: Type) (tree :: Kind.Tree) | p d -> tree where
  root :: TTree p tree

-- TODO undefined
instance (
    TableMeta d meta,
    Tree p d meta c
  ) => Root p d c where
  root =
    tree @p @d @meta undefined
