module Polysemy.Db.Tree.Unfold where

import Generics.SOP (AllZip, HTrans (htrans), I(I), K(K), LiftedCoercible, NP(Nil, (:*)), NS (Z, S), POP(POP), SListI, hcoerce, hindex, hpure, unI, unSOP, unZ)
import Generics.SOP.GGP (gfrom)

import Polysemy.Db.Data.Column (Con(Con), Prim, PrimQuery, Rep)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import Polysemy.Db.Data.Uid (Uid)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Constraint (ConstructSOP, DataName, Top2, symbolText)
import Polysemy.Db.SOP.Fundeps (Fundep, Fundeps)
import Polysemy.Db.SOP.List (NPAsUnit (npAsUnit))
import Polysemy.Db.Text.DbIdentifier (quotedDbId)
import Polysemy.Db.Tree.Data.Effect (ADT)
import Polysemy.Db.Tree.Effect (TreeEffects)
import Polysemy.Db.Tree.Meta (
  AdtMetadata (AdtProd, AdtSum),
  ConMeta(ConMeta),
  ConMetaTypes,
  ConsMetaTypes,
  ProdDefaultRep,
  TreeMeta(TreeMeta),
  TreeMetaType,
  TreeMetaTypes,
  )
import qualified Polysemy.Db.Type.Data.Tree as Type

data Params =
  Params {
    tag :: Type,
    treeParam :: Type,
    nodeParam :: Type -> Type,
    payload :: Type -> Type
  }

type family Tag (params :: Params) :: Type where
  Tag ('Params tag _ _ _) = tag

type family TreeParam (params :: Params) :: Type where
  TreeParam ('Params _ tree _ _) = tree

type family NodeParam (params :: Params) :: Type -> Type where
  NodeParam ('Params _ _ node _) = node

type family Payload (params :: Params) :: Type -> Type where
  Payload ('Params _ _ _ pf) = pf

type family PayloadT (params :: Params) (meta :: TreeMeta) :: Type where
  PayloadT p meta = Payload p (TreeMetaType meta)

type family TTree (params :: Params) :: Kind.Tree -> Type where
  TTree ('Params _ t n _) = Type.Tree t n

type family TNode (params :: Params) :: Kind.Node -> Type where
  TNode ('Params _ t n _) = Type.Node t n

class TreePrim (tag :: Type) (n :: Type -> Type) (a :: Type) (name :: FieldId) (d :: Type) where
  treePrim :: a -> n d

instance {-# overlappable #-} Applicative f => TreePrim tag f d name d where
  treePrim =
    pure

instance {-# overlappable #-} TreePrim tag f (f d) name d where
  treePrim =
    id

instance {-# incoherent #-} (
    Applicative f,
    ConstructSOP a ass
  ) => TreePrim tag f a ('NamedField "sum_index") Int where
  treePrim =
    pure . hindex . gfrom

class TreePayload (tag :: Type) (t :: Type) (a :: Type) (meta :: TreeMeta) (effs :: [Type]) where
  treePayload :: a -> t

instance {-# overlappable #-} TreePayload tag () a meta effs where
  treePayload _ =
    ()

class TreeProduct (tag :: Type) (f :: Type -> Type) (metas :: [TreeMeta]) (d :: Type) where
  treeProduct :: d -> NP f (TreeMetaTypes metas)

instance {-# overlappable #-} SListI (TreeMetaTypes metas) => TreeProduct tag (K ()) metas (K () d) where
  treeProduct _ =
    hpure (K ())

instance (
    ds ~ TreeMetaTypes metas,
    ConstructSOP d '[ds]
  ) => TreeProduct tag I metas (I d) where
    treeProduct =
      unZ . unSOP . gfrom . unI

class TreeSum (tag :: Type) (f :: Type -> Type) (metas :: [ConMeta]) (d :: Type) where
  treeSum :: f d -> POP f (ConsMetaTypes metas)

class TreeConProduct (tag :: Type) (f :: Type -> Type) (metas :: [TreeMeta]) (d :: Type) where
  treeConProduct :: d -> NP f (TreeMetaTypes metas)

instance {-# overlappable #-} (
    NPAsUnit f metas (TreeMetaTypes metas)
  ) => TreeConProduct tag f metas () where
  treeConProduct _ =
    npAsUnit @_ @f @metas

class TreeConPayload (tag :: Type) (field :: FieldId) (t :: Type) where
  treeConPayload :: t

------------------------------------------------------------------------------------------------------------------------

-- TODO use TM for the payload in Tree
newtype TM (f :: Type -> Type) (meta :: TreeMeta) =
  TM { unTM :: f (TreeMetaType meta) }

class CoerceTM (f :: Type -> Type) (metas :: [TreeMeta]) where
  coerceTM :: NP f (TreeMetaTypes metas) -> NP (TM f) metas

instance {-# overlappable #-} (
    AllZip (LiftedCoercible f (TM f)) (TreeMetaTypes metas) metas
  ) => CoerceTM f metas where
  coerceTM =
    hcoerce

instance (
  AllZip Top2 (TreeMetaTypes metas) metas
  ) => CoerceTM (K ()) metas where
  coerceTM =
    htrans (Proxy @Top2) \ (K x) -> TM (K x)

------------------------------------------------------------------------------------------------------------------------

class ProdTrees (p :: Params) (metas :: [TreeMeta]) (trees :: [Kind.Tree]) | p metas -> trees where
  prodTrees :: NP (Payload p) (TreeMetaTypes metas) -> NP (TTree p) trees

instance (
    f ~ Payload p,
    AllZip (LiftedCoercible f (TM f)) (TreeMetaTypes metas) metas,
    Fundeps (Fundep (Tree p)) metas trees
  ) => ProdTrees p metas trees where
    prodTrees prod =
      htrans (Proxy @(Tree p)) f (coerceTM @f @metas prod)
      where
        f :: ∀ meta tree . Tree p meta tree => TM f meta -> TTree p tree
        f (TM x) =
          tree @p @meta @tree x

------------------------------------------------------------------------------------------------------------------------

class ConTree (p :: Params) (meta :: ConMeta) (tree :: Kind.Tree) | p meta -> tree where
  conTree :: NP (Payload p) (ConMetaTypes meta) -> TTree p tree

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
    meta ~ 'TreeMeta cname rep d,
    con ~ Payload p d,
    Tree p meta tree
  ) => ConTree p ('ConMeta cname '[ 'TreeMeta name rep d]) tree where
    conTree (payload :* Nil) =
      tree @p @meta payload

------------------------------------------------------------------------------------------------------------------------

class SumProdTrees (p :: Params) (metas :: [ConMeta]) (trees :: [Kind.Tree]) | p metas -> trees where
  sumProdTrees :: POP (Payload p) (ConsMetaTypes metas) -> NP (TTree p) trees

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

class SumConTree (p :: Params) (meta :: ConMeta) (tree :: Kind.Tree) | p meta -> tree where
  sumConTree :: NP I (ConMetaTypes meta) -> TTree p tree

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

class SumTrees (p :: Params) (dss :: [[*]]) (metas :: [ConMeta]) (trees :: [Kind.Tree]) | p dss metas -> trees where
  sumTrees :: NS (NP I) dss -> NS (TTree p) trees

instance SumTrees p '[] '[] '[] where
  sumTrees _ =
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

class ProdForSumTree (tag :: Type) (decision :: Bool) | tag -> decision where

class SumNode (p :: Params) (sum :: Bool) (d :: Type) (metas :: [ConMeta]) (node :: Kind.Node) | p sum d metas -> node where
  sumNode :: Payload p d -> TNode p node

type SumIndex =
  'Kind.Tree ('NamedField "sum_index") '[Prim] ('Kind.Prim Int)

instance (
    TreeSum tag f metas d,
    sumName ~ 'NamedField "sum",
    TreePrim tag n (f d) sumName d,
    indexMeta ~ 'TreeMeta ('NamedField "sum_index") Prim Int,
    SumProdTrees p metas trees,
    indexParams ~ 'Params tag t n (K ()),
    Tree indexParams indexMeta SumIndex,
    node ~ 'Kind.Prod d (SumIndex : trees),
    p ~ 'Params tag t n f
  ) => SumNode p 'False d metas node where
  sumNode fd =
    Type.Prod (treePrim @tag @n @(f d) @sumName @d fd) (indexTree :* sumProdTrees @p @metas (treeSum @tag @f @metas fd))
    where
      indexTree =
        tree @indexParams @indexMeta (K ())

instance (
    p ~ 'Params tag t n I,
    sumName ~ 'NamedField "sum",
    TreePrim tag n (I d) sumName d,
    ConstructSOP d dss,
    SumTrees p dss metas trees,
    node ~ 'Kind.Sum d trees
  ) => SumNode p 'True d metas node where
    sumNode (I d) =
      Type.Sum (treePrim @tag @n @(I d) @sumName @d (I d)) (sumTrees @p @_ @metas (unSOP (gfrom d)))

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
  tree :: PayloadT p meta -> TTree p tree

instance (
    TreeEffects tag rep d effs,
    TreePayload tag t (f d) meta effs,
    meta ~ 'TreeMeta name rep d,
    Node p name d effs node,
    p ~ 'Params tag t n f
  ) => Tree p meta ('Kind.Tree name effs node) where
    tree a =
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
    tree @p @meta
