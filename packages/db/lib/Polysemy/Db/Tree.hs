module Polysemy.Db.Tree where

import Generics.SOP (I(I), NP(Nil, (:*)), NS (Z, S), unSOP)
import Generics.SOP.GGP (gfrom)

import Polysemy.Db.Data.Column (Con(Con), Prim, PrimQuery, Rep)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import Polysemy.Db.Data.Uid (Uid)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Constraint (ConstructSOP, DataName, symbolText)
import Polysemy.Db.SOP.List (NPAsUnit (npAsUnit))
import Polysemy.Db.Text.DbIdentifier (quotedDbId)
import Polysemy.Db.Tree.Data.Effect (ADT)
import Polysemy.Db.Tree.Effect (TreeEffects)
import Polysemy.Db.Tree.Meta (AdtMetadata (AdtProd, AdtSum), ConMeta, ConMeta(ConMeta), ConMetaTypes, ProdDefaultRep, TreeMeta(TreeMeta), TreeMetaTypes)
import qualified Polysemy.Db.Type.Data.Tree as Type

class A a b | a -> b where

class ADeps (as :: [*]) (bs :: [*]) | as -> bs where

instance ADeps '[] '[] where

instance (
    A a b,
    ADeps as bs
  ) => ADeps (a : as) (b : bs) where

class B (as :: [*]) (bs :: [*]) | as -> bs where

instance ADeps as bs => B as bs where

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

instance {-# overlappable #-} TreePayload tag () a meta effs where
  treePayload _ =
    ()

class TreeProduct (tag :: Type) (metas :: [TreeMeta]) (d :: Type) (prod :: [Type]) | tag metas d -> prod where
  treeProduct :: d -> NP I prod

instance {-# overlappable #-} (
    NPAsUnit metas ds
  ) => TreeProduct tag metas () ds where
  treeProduct _ =
    npAsUnit @_ @metas

class TreeConProduct (tag :: Type) (metas :: [TreeMeta]) (d :: Type) (prod :: [Type]) | tag metas d -> prod where
  treeConProduct :: d -> NP I prod

instance {-# overlappable #-} (
    NPAsUnit metas ds
  ) => TreeConProduct tag metas () ds where
  treeConProduct _ =
    npAsUnit @_ @metas

class TreeConPayload (tag :: Type) (field :: FieldId) (t :: Type) where
  treeConPayload :: t

class TreeCons (tag :: Type) (d :: Type) (cons :: Type) | tag d -> cons where
  treeCons :: d -> cons

class TreeConElem (tag :: Type) (cons :: Type) (consTail :: Type) (con :: Type) | tag cons -> consTail con where
  treeConElem :: cons -> (consTail, con)

instance TreeConElem tag () () () where
  treeConElem () =
    ((), ())

------------------------------------------------------------------------------------------------------------------------

-- TODO parameterize the `I`, maybe the same as `n`?
class ProdTrees (p :: Params) (prod :: [Type]) (metas :: [TreeMeta]) (trees :: [Kind.Tree]) | p prod metas -> trees where
  prodTrees :: NP I prod -> NP (TTree p) trees

instance ProdTrees p '[] '[] '[] where
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
    Applicative n,
    p ~ 'Params tag t n,
    TreeConPayload tag field t,
    TreeConProduct tag metas con prod,
    ProdTrees p prod metas trees,
    tree ~ 'Kind.Tree field '[] ('Kind.Prod (Con field) trees)
  ) => ConTree p con ('ConMeta field metas) tree where
    conTree con =
      Type.Tree (treeConPayload @tag @field) (Type.Prod (pure Con) (prodTrees @p @prod @metas (treeConProduct @tag @metas @con con)))

instance (
    meta ~ 'TreeMeta cname rep d,
    Tree p con meta tree
  ) => ConTree p con ('ConMeta cname '[ 'TreeMeta name rep d]) tree where
    conTree =
      tree @p @_ @meta

------------------------------------------------------------------------------------------------------------------------

class SumProdTrees (p :: Params) (cons :: Type) (metas :: [ConMeta]) (trees :: [Kind.Tree]) | p cons metas -> trees where
  sumProdTrees :: cons -> NP (TTree p) trees

instance SumProdTrees p cons '[] '[] where
  sumProdTrees _ =
    Nil

instance (
    TreeConElem (Tag p) consData consTail conData,
    ConTree p conData meta tree,
    SumProdTrees p consTail metas trees
  ) => SumProdTrees p consData (meta : metas) (tree : trees) where
    sumProdTrees cons =
      conTree @p @_ @meta con :* sumProdTrees @p @_ @metas conTail
      where
        (conTail, con) =
          treeConElem @(Tag p) cons

------------------------------------------------------------------------------------------------------------------------

class SumConTree (p :: Params) (meta :: ConMeta) (tree :: Kind.Tree) | p meta -> tree where
  sumConTree :: NP I (ConMetaTypes meta) -> TTree p tree

instance (
    Applicative n,
    p ~ 'Params tag t n,
    prod ~ TreeMetaTypes nodes,
    TreeConPayload tag field t,
    ProdTrees p prod nodes node,
    tree ~ 'Kind.Tree field '[] ('Kind.Prod (Con field) node)
  ) => SumConTree p ('ConMeta field nodes) tree where
    sumConTree con =
      Type.Tree (treeConPayload @tag @field) (Type.Prod (pure Con) (prodTrees @p @prod @nodes con))

------------------------------------------------------------------------------------------------------------------------

class SumTrees (p :: Params) (dss :: [[*]]) (metas :: [ConMeta]) (trees :: [Kind.Tree]) | p dss metas -> trees where
  sumTrees :: NS (NP I) dss -> NS (TTree p) trees

instance SumTrees p '[] '[] '[] where
  sumTrees =
    undefined

instance (
    ds ~ ConMetaTypes meta,
    SumConTree p meta tree,
    SumTrees p dss metas trees
  ) => SumTrees p (ds : dss) (meta : metas) (tree : trees) where
  sumTrees = \case
    Z con -> Z (sumConTree @p @meta @tree con)
    S cons -> S (sumTrees @p @dss @metas @trees cons)

------------------------------------------------------------------------------------------------------------------------

class SumOrProd (tag :: Type) (decision :: Bool) | tag -> decision where

class SumNode (p :: Params) (sum :: Bool) (d :: Type) (a :: Type) (metas :: [ConMeta]) (node :: Kind.Node) | p sum d a metas -> node where
  sumNode :: a -> TNode p node

type SumIndexTree =
  'Kind.Tree ('NamedField "sum_index") '[Prim] ('Kind.Prim Int)

instance (
    TreeCons tag a cons,
    indexName ~ 'NamedField "sum_index",
    sumName ~ 'NamedField "sum",
    TreePrim tag n a sumName d,
    indexMeta ~ 'TreeMeta indexName Prim Int,
    TreePrim tag n a indexName Int,
    TreePayload tag t () indexMeta '[Prim],
    SumProdTrees p cons metas trees,
    node ~ 'Kind.Prod d (SumIndexTree : trees),
    p ~ 'Params tag t n
  ) => SumNode p 'False d a metas node where
  sumNode a =
    Type.Prod (treePrim @tag @n @a @sumName @d a) (indexTree :* sumProdTrees @p @_ @metas (treeCons @tag a))
    where
      indexTree =
        Type.Tree indexPayload (Type.Prim (treePrim @tag @_ @_ @indexName @Int a))
      indexPayload =
        treePayload @tag @_ @_ @indexMeta @'[Prim] ()

instance (
    p ~ 'Params tag t n,
    sumName ~ 'NamedField "sum",
    TreePrim tag n d sumName d,
    ConstructSOP d dss,
    SumTrees p dss metas trees,
    node ~ 'Kind.Sum d trees
  ) => SumNode p 'True d d metas node where
    sumNode d =
      Type.Sum (treePrim @tag @n @d @sumName @d d) (sumTrees @p @_ @metas (unSOP (gfrom d)))

------------------------------------------------------------------------------------------------------------------------

class AdtTree (p :: Params) (d :: Type) (a :: Type) (meta :: AdtMetadata) (eff :: [Type]) (node :: Kind.Node) | p d a meta eff -> node where
  adtTree :: a -> TNode p node

instance (
    TreeProduct tag metas a prod,
    prodName ~ 'NamedField "prod",
    TreePrim tag n a prodName d,
    ProdTrees p prod metas trees,
    p ~ 'Params tag t n
  ) => AdtTree p d a ('AdtProd metas) '[] ('Kind.Prod d trees) where
  adtTree a =
    Type.Prod (treePrim @tag @n @a @prodName @d a) (prodTrees @p @prod @metas (treeProduct @tag @metas a))

instance (
    p ~ 'Params tag t n,
    SumOrProd tag sum,
    SumNode p sum d a metas node
  ) => AdtTree p d a ('AdtSum metas) '[] node where
  adtTree =
    sumNode @p @sum @d @a @metas

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
    TreeEffects tag rep d effs,
    TreePayload tag t a meta effs,
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
